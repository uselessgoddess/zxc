use {
    crate::codegen::llvm::{mir::is_rvalue_produce_operand, value, FunctionCx},
    middle::{
        graph::Dominators,
        mir::{
            traversal,
            visit::{MutatingUseContext, NonMutatingUseContext, PlaceContext, Visitor},
            BasicBlock, DefLocation, Local, Location, Place, PlaceElem, PlaceRef, Rvalue,
        },
        BitSet, IndexVec,
    },
};

#[derive(Copy, Clone, PartialEq, Eq)]
enum LocalKind {
    ZST,
    Memory,
    Unused,
    SSA(DefLocation),
}

pub fn non_ssa(fx: &FunctionCx<'_, '_, '_>) -> BitSet<Local> {
    let mir = fx.mir;
    let dominators = mir.basic_blocks.dominators();
    let locals = mir
        .local_decls
        .iter()
        .map(|decl| {
            let abi = fx.cx.tcx.layout_of(decl.ty);
            if abi.is_zst() {
                LocalKind::ZST
            } else if value::is_llvm_immediate(abi.layout) {
                LocalKind::Unused
            } else {
                LocalKind::Memory
            }
        })
        .collect();

    let mut analyzer = LocalAnalyzer { fx, dominators, locals };

    for arg in mir.args_iter() {
        analyzer.assign(arg, DefLocation::Argument);
    }

    for (bb, data) in traversal::reverse_postorder(&mir) {
        analyzer.visit_basic_block_data(bb, data);
    }

    let mut non_ssa_locals = BitSet::new_empty(analyzer.locals.len());
    for (local, &kind) in analyzer.locals.iter_enumerated() {
        if kind == LocalKind::Memory {
            non_ssa_locals.insert(local);
        }
    }

    non_ssa_locals
}

struct LocalAnalyzer<'mir, 'a, 'll, 'tcx> {
    fx: &'mir FunctionCx<'a, 'll, 'tcx>,
    dominators: Dominators<BasicBlock>,
    locals: IndexVec<Local, LocalKind>,
}

impl<'mir, 'a, 'll, 'tcx> LocalAnalyzer<'mir, 'a, 'll, 'tcx> {
    fn assign(&mut self, local: Local, location: DefLocation) {
        let kind = &mut self.locals[local];
        match *kind {
            LocalKind::ZST | LocalKind::Memory => {}
            LocalKind::Unused => *kind = LocalKind::SSA(location),
            LocalKind::SSA(_) => *kind = LocalKind::Memory,
        }
    }

    fn process_place(
        &mut self,
        place_ref: &PlaceRef<'tcx>,
        context: PlaceContext,
        location: Location,
    ) {
        let tcx = self.fx.cx.tcx;

        if let Some((place_base, elem)) = place_ref.last_projection() {
            let mut base_context = if context.is_mutating_use() {
                PlaceContext::MutatingUse(MutatingUseContext::Projection)
            } else {
                PlaceContext::NonMutatingUse(NonMutatingUseContext::Projection)
            };

            // is consume
            if let PlaceContext::NonMutatingUse(
                NonMutatingUseContext::Copy | NonMutatingUseContext::Move,
            ) = context
            {
                let base_ty = place_base.ty(tcx, &self.fx.mir.local_decls);
                let elem_ty = PlaceRef::projection_ty(tcx, base_ty, elem);

                if self.fx.cx.tcx.layout_of(elem_ty).is_zst() {
                    return;
                }
            }

            if let PlaceElem::Deref = elem {
                base_context = PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy);
            }

            self.process_place(&place_base, base_context, location);
        } else {
            self.visit_local(place_ref.local, context, location);
        }
    }
}

impl<'tcx> Visitor<'tcx> for LocalAnalyzer<'_, '_, '_, 'tcx> {
    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>, location: Location) {
        if let Some(local) = place.as_local() {
            self.assign(local, DefLocation::Body(location));
            if self.locals[local] != LocalKind::Memory {
                if !is_rvalue_produce_operand(self.fx, *rvalue) {
                    self.locals[local] = LocalKind::Memory;
                }
            }
        } else {
            self.visit_place(place, PlaceContext::MutatingUse(MutatingUseContext::Store), location);
        }

        self.visit_rvalue(rvalue, location);
    }

    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        self.process_place(&place.as_ref(), context, location);
    }

    fn visit_local(&mut self, local: Local, context: PlaceContext, location: Location) {
        match context {
            PlaceContext::MutatingUse(MutatingUseContext::Call)
            | PlaceContext::MutatingUse(MutatingUseContext::Yield) => {
                self.assign(local, DefLocation::Body(location));
            }

            PlaceContext::NonMutatingUse(NonMutatingUseContext::PlaceMention)
            | PlaceContext::MutatingUse(MutatingUseContext::Retag) => unreachable!(),

            PlaceContext::NonMutatingUse(
                NonMutatingUseContext::Copy | NonMutatingUseContext::Move,
            ) => match &mut self.locals[local] {
                LocalKind::ZST | LocalKind::Memory => {}
                LocalKind::SSA(def) if def.dominates(location, &self.dominators) => {}
                // Reads from uninitialized variables (e.g., in dead code, after
                // optimizations) require locals to be in (uninitialized) memory.
                // N.B., there can be uninitialized reads of a local visited after
                // an assignment to that local, if they happen on disjoint paths.
                kind @ (LocalKind::Unused | LocalKind::SSA(_)) => {
                    *kind = LocalKind::Memory;
                }
            },

            PlaceContext::MutatingUse(
                MutatingUseContext::Store
                | MutatingUseContext::Deinit
                | MutatingUseContext::SetDiscriminant
                | MutatingUseContext::AsmOutput
                | MutatingUseContext::Borrow
                | MutatingUseContext::AddressOf
                | MutatingUseContext::Projection,
            )
            | PlaceContext::NonMutatingUse(
                NonMutatingUseContext::Inspect
                | NonMutatingUseContext::SharedBorrow
                | NonMutatingUseContext::FakeBorrow
                | NonMutatingUseContext::AddressOf
                | NonMutatingUseContext::Projection,
            ) => {
                self.locals[local] = LocalKind::Memory;
            }

            PlaceContext::MutatingUse(MutatingUseContext::Drop) => {
                let kind = &mut self.locals[local];
                if *kind != LocalKind::Memory {
                    // The semantics of `PlaceContext` are borrowed from Rust,
                    // but there is nothing like `Drop` right now
                }
            }
        }
    }
}
