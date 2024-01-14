use {
    crate::{
        fx::FxIndexSet,
        mir::{
            traversal,
            visit::{MutVisitor, MutatingUseContext, PlaceContext, Visitor},
            BasicBlock, BasicBlockData, Body, Local, Location, MirPass, Place, Statement,
            SwitchTargets, Terminator, START_BLOCK,
        },
        Idx, IndexSlice, IndexVec, Session, Tx,
    },
    smallvec::SmallVec,
};

pub enum SimplifyCfg {
    EarlyOpt,
    Final,
}

impl<'tcx> MirPass<'tcx> for SimplifyCfg {
    fn name(&self) -> &'static str {
        match self {
            SimplifyCfg::EarlyOpt => "SimplifyCfg-early-opt",
            SimplifyCfg::Final => "SimplifyCfg-final",
        }
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        simplify_cfg(tcx, body)
    }
}

pub fn simplify_cfg<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
    CfgSimplifier::new(body).simplify();
    remove_dead_blocks(body);
}

pub fn simplify_duplicate_switch_targets(terminator: &mut Terminator<'_>) {
    if let Terminator::SwitchInt { targets, .. } = terminator {
        let otherwise = targets.otherwise();
        if targets.iter().any(|t| t.1 == otherwise) {
            *targets = SwitchTargets::new(
                targets.iter().filter(|t| t.1 != otherwise),
                targets.otherwise(),
            );
        }
    }
}

pub fn remove_duplicate_unreachable_blocks<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
    struct OptApplier<'tcx> {
        tcx: Tx<'tcx>,
        duplicates: FxIndexSet<BasicBlock>,
    }

    impl<'tcx> MutVisitor<'tcx> for OptApplier<'tcx> {
        fn tcx(&self) -> Tx<'tcx> {
            self.tcx
        }

        fn visit_terminator(&mut self, terminator: &mut Terminator<'tcx>, location: Location) {
            for target in terminator.successors_mut() {
                if self.duplicates.contains(target) {
                    *target = self.duplicates[0];
                }
            }

            simplify_duplicate_switch_targets(terminator);

            self.super_terminator(terminator, location);
        }
    }

    let unreachable_blocks = body
        .basic_blocks
        .iter_enumerated()
        .filter(|(_, bb)| {
            // CfgSimplifier::simplify leaves behind some unreachable basic blocks without a
            // terminator. Those blocks will be deleted by remove_dead_blocks, but we run just
            // before then so we need to handle missing terminators.
            // We also need to prevent confusing cleanup and non-cleanup blocks. In practice we
            // don't emit empty unreachable cleanup blocks, so this simple check suffices.
            bb.terminator.is_some() && bb.is_empty_unreachable()
        })
        .map(|(block, _)| block)
        .collect::<FxIndexSet<_>>();

    if unreachable_blocks.len() > 1 {
        OptApplier { tcx, duplicates: unreachable_blocks }.visit_body(body);
    }
}

pub fn remove_dead_blocks(body: &mut Body<'_>) {
    let reachable = traversal::reachable_as_bitset(body);
    let num_blocks = body.basic_blocks.len();
    if num_blocks == reachable.count() {
        return;
    }

    let basic_blocks = &mut body.basic_blocks;

    let mut replacements: Vec<_> = (0..num_blocks).map(BasicBlock::new).collect();
    let mut orig_index = 0;
    let mut used_index = 0;
    basic_blocks.raw.retain(|_| {
        let keep = reachable.contains(BasicBlock::new(orig_index));
        if keep {
            replacements[orig_index] = BasicBlock::new(used_index);
            used_index += 1;
        }
        orig_index += 1;
        keep
    });

    for block in basic_blocks {
        for target in block.terminator_mut().successors_mut() {
            *target = replacements[target.index()];
        }
    }
}

pub struct CfgSimplifier<'a, 'tcx> {
    basic_blocks: &'a mut IndexSlice<BasicBlock, BasicBlockData<'tcx>>,
    pred_count: IndexVec<BasicBlock, u32>,
}

impl<'a, 'tcx> CfgSimplifier<'a, 'tcx> {
    pub fn new(body: &'a mut Body<'tcx>) -> Self {
        let mut pred_count = IndexVec::from_elem(0u32, &body.basic_blocks);

        // we can't use mir.predecessors() here because that counts
        // dead blocks, which we don't want to.
        pred_count[START_BLOCK] = 1;

        for (_, data) in traversal::preorder(body) {
            if let Some(ref term) = data.terminator {
                for tgt in term.successors() {
                    pred_count[tgt] += 1;
                }
            }
        }

        let basic_blocks = &mut body.basic_blocks;
        CfgSimplifier { basic_blocks, pred_count }
    }

    fn strip_nops(&mut self) {
        for blk in self.basic_blocks.iter_mut() {
            blk.statements.retain(|stmt| !matches!(stmt, Statement::Nop))
        }
    }

    pub fn simplify(mut self) {
        self.strip_nops();

        let mut merged_blocks = Vec::new();
        loop {
            let mut changed = false;

            for bb in self.basic_blocks.indices() {
                if self.pred_count[bb] == 0 {
                    continue;
                }

                let mut terminator =
                    self.basic_blocks[bb].terminator.take().expect("invalid terminator state");

                for successor in terminator.successors_mut() {
                    self.collapse_goto_chain(successor, &mut changed);
                }

                let mut inner_changed = true;
                merged_blocks.clear();
                while inner_changed {
                    inner_changed = false;
                    inner_changed |= self.simplify_branch(&mut terminator);
                    inner_changed |= self.merge_successor(&mut merged_blocks, &mut terminator);
                    changed |= inner_changed;
                }

                let statements_to_merge =
                    merged_blocks.iter().map(|&i| self.basic_blocks[i].statements.len()).sum();

                if statements_to_merge > 0 {
                    let mut statements = std::mem::take(&mut self.basic_blocks[bb].statements);
                    statements.reserve(statements_to_merge);
                    for &from in &merged_blocks {
                        statements.append(&mut self.basic_blocks[from].statements);
                    }
                    self.basic_blocks[bb].statements = statements;
                }

                self.basic_blocks[bb].terminator = Some(terminator);
            }

            if !changed {
                break;
            }
        }
    }

    fn take_terminator_if_simple_goto(&mut self, bb: BasicBlock) -> Option<Terminator<'tcx>> {
        match self.basic_blocks[bb] {
            BasicBlockData {
                ref statements,
                terminator: ref mut terminator @ Some(Terminator::Goto { .. }),
                ..
            } if statements.is_empty() => terminator.take(),
            // if `terminator` is None, this means we are in a loop. In that
            // case, let all the loop collapse to its entry.
            _ => None,
        }
    }

    fn collapse_goto_chain(&mut self, start: &mut BasicBlock, changed: &mut bool) {
        let mut terminators: SmallVec<_, 1> = Default::default();
        let mut current = *start;
        while let Some(terminator) = self.take_terminator_if_simple_goto(current) {
            let Terminator::Goto { target } = terminator else {
                unreachable!();
            };
            terminators.push((current, terminator));
            current = target;
        }
        let last = current;
        *start = last;
        while let Some((current, mut terminator)) = terminators.pop() {
            let Terminator::Goto { ref mut target } = terminator else {
                unreachable!();
            };
            *changed |= *target != last;
            *target = last;

            if self.pred_count[current] == 1 {
                // This is the last reference to current, so the pred-count to
                // to target is moved into the current block.
                self.pred_count[current] = 0;
            } else {
                self.pred_count[*target] += 1;
                self.pred_count[current] -= 1;
            }
            self.basic_blocks[current].terminator = Some(terminator);
        }
    }

    // turn a branch with all successors identical to a goto
    fn simplify_branch(&mut self, terminator: &mut Terminator<'tcx>) -> bool {
        match terminator {
            Terminator::SwitchInt { .. } => {}
            _ => return false,
        };

        let first_succ = {
            if let Some(first_succ) = terminator.successors().next() {
                if terminator.successors().all(|s| s == first_succ) {
                    let count = terminator.successors().count();
                    self.pred_count[first_succ] -= (count - 1) as u32;
                    first_succ
                } else {
                    return false;
                }
            } else {
                return false;
            }
        };

        *terminator = Terminator::Goto { target: first_succ };
        true
    }

    // merge a block with 1 `goto` predecessor to its parent
    fn merge_successor(
        &mut self,
        merged_blocks: &mut Vec<BasicBlock>,
        terminator: &mut Terminator<'tcx>,
    ) -> bool {
        let target = match *terminator {
            Terminator::Goto { target } if self.pred_count[target] == 1 => target,
            _ => return false,
        };

        *terminator = match self.basic_blocks[target].terminator.take() {
            Some(terminator) => terminator,
            None => {
                // unreachable loop - this should not be possible, as we
                // don't strand blocks, but handle it correctly.
                return false;
            }
        };

        merged_blocks.push(target);
        self.pred_count[target] = 0;

        true
    }
}

struct UsedLocals {
    increment: bool,
    argc: u32,
    use_count: IndexVec<Local, u32>,
}

impl UsedLocals {
    /// Determines which locals are used & unused in the given body.
    fn new(body: &Body<'_>) -> Self {
        let mut this = Self {
            increment: true,
            argc: body.argc.try_into().unwrap(),
            use_count: IndexVec::from_elem(0, &body.local_decls),
        };
        this.visit_body(body);
        this
    }

    /// Checks if local is used.
    ///
    /// Return place and arguments are always considered used.
    fn is_used(&self, local: Local) -> bool {
        local.as_u32() <= self.argc || self.use_count[local] != 0
    }

    /// Updates the use counts to reflect the removal of given statement.
    fn statement_removed(&mut self, statement: &Statement<'_>) {
        self.increment = false;

        // The location of the statement is irrelevant.
        let location = Location::START;
        self.visit_statement(statement, location);
    }

    /// Visits a left-hand side of an assignment.
    fn visit_lhs(&mut self, place: &Place<'_>, location: Location) {
        if place.is_indirect() {
            // A use, not a definition.
            self.visit_place(place, PlaceContext::MutatingUse(MutatingUseContext::Store), location);
        } else {
            // A definition. The base local itself is not visited, so this occurrence is not counted
            // toward its use count. There might be other locals still, used in an indexing
            // projection.
            self.super_projection(
                place.as_ref(),
                PlaceContext::MutatingUse(MutatingUseContext::Projection),
                location,
            );
        }
    }
}

impl<'tcx> Visitor<'tcx> for UsedLocals {
    fn visit_statement(&mut self, statement: &Statement<'tcx>, location: Location) {
        match statement {
            Statement::Nop => {}
            Statement::Assign(place, rvalue) => {
                if rvalue.is_safe_to_remove() {
                    self.visit_lhs(place, location);
                    self.visit_rvalue(rvalue, location);
                } else {
                    self.super_statement(statement, location);
                }
            }
        }
    }

    fn visit_local(&mut self, local: Local, _ctx: PlaceContext, _location: Location) {
        if self.increment {
            self.use_count[local] += 1;
        } else {
            assert_ne!(self.use_count[local], 0);
            self.use_count[local] -= 1;
        }
    }
}

pub enum SimplifyLocals {
    AfterGVN,
    Final,
}

impl<'tcx> MirPass<'tcx> for SimplifyLocals {
    fn name(&self) -> &'static str {
        match &self {
            SimplifyLocals::AfterGVN => "SimplifyLocals-after-value-numbering",
            SimplifyLocals::Final => "SimplifyLocals-final",
        }
    }

    fn is_enabled(&self, sess: &Session) -> bool {
        sess.mir_opt_level() > 0
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        simplify_locals(tcx, body);
    }
}

fn remove_unused_definitions_helper(used_locals: &mut UsedLocals, body: &mut Body<'_>) {
    let mut modified = true;

    while modified {
        modified = false;

        for data in &mut body.basic_blocks {
            data.statements.retain(|statement| {
                let keep = match statement {
                    Statement::Assign(place, _) => used_locals.is_used(place.local),
                    Statement::Nop => false,
                    _ => true,
                };

                if !keep {
                    modified = true;
                    used_locals.statement_removed(statement);
                }

                keep
            });
        }
    }
}

fn make_local_map<V>(
    local_decls: &mut IndexVec<Local, V>,
    used_locals: &UsedLocals,
) -> IndexVec<Local, Option<Local>> {
    let mut map: IndexVec<Local, Option<Local>> = IndexVec::from_elem(None, local_decls);
    let mut used = Local::new(0);

    for alive_index in local_decls.indices() {
        // `is_used` treats the `RETURN_PLACE` and arguments as used.
        if !used_locals.is_used(alive_index) {
            continue;
        }

        map[alive_index] = Some(used);
        if alive_index != used {
            local_decls.swap(alive_index, used);
        }
        used.increment_by(1);
    }
    local_decls.truncate(used.index());
    map
}

pub fn simplify_locals<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
    // First, we're going to get a count of *actual* uses for every `Local`.
    let mut used_locals = UsedLocals::new(body);

    // Next, we're going to remove any `Local` with zero actual uses. When we remove those
    // `Locals`, we're also going to subtract any uses of other `Locals` from the `used_locals`
    // count. For example, if we removed `_2 = discriminant(_1)`, then we'll subtract one from
    // `use_counts[_1]`. That in turn might make `_1` unused, so we loop until we hit a
    // fixedpoint where there are no more unused locals.
    remove_unused_definitions_helper(&mut used_locals, body);

    // Finally, we'll actually do the work of shrinking `body.local_decls`
    // and remapping the `Local`s.
    let map = make_local_map(&mut body.local_decls, &used_locals);

    // Only bother running the `LocalUpdater` if we actually found locals to remove.
    if map.iter().any(Option::is_none) {
        // Update references to all vars and tmps now
        let mut updater = LocalUpdater { map, tcx };
        updater.visit_body_preserves_cfg(body);

        body.local_decls.shrink_to_fit();
    }
}

struct LocalUpdater<'tcx> {
    map: IndexVec<Local, Option<Local>>,
    tcx: Tx<'tcx>,
}

impl<'tcx> MutVisitor<'tcx> for LocalUpdater<'tcx> {
    fn tcx(&self) -> Tx<'tcx> {
        self.tcx
    }

    fn visit_local(&mut self, l: &mut Local, _: PlaceContext, _: Location) {
        *l = self.map[*l].unwrap();
    }
}
