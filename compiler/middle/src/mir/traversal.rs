use crate::{
    idx::BitSet,
    mir::{syntax::Successors, BasicBlock, BasicBlockData, Body, START_BLOCK},
    IndexSlice,
};

#[derive(Clone)]
pub struct Preorder<'a, 'tcx> {
    body: &'a Body<'tcx>,
    visited: BitSet<BasicBlock>,
    worklist: Vec<BasicBlock>,
    root_is_start_block: bool,
}

impl<'a, 'tcx> Preorder<'a, 'tcx> {
    pub fn new(body: &'a Body<'tcx>, root: BasicBlock) -> Preorder<'a, 'tcx> {
        let worklist = vec![root];

        Preorder {
            body,
            visited: BitSet::new_empty(body.basic_blocks.len()),
            worklist,
            root_is_start_block: root == START_BLOCK,
        }
    }
}

impl<'a, 'tcx> Iterator for Preorder<'a, 'tcx> {
    type Item = (BasicBlock, &'a BasicBlockData<'tcx>);

    fn next(&mut self) -> Option<(BasicBlock, &'a BasicBlockData<'tcx>)> {
        while let Some(idx) = self.worklist.pop() {
            if !self.visited.insert(idx) {
                continue;
            }

            let data = &self.body.basic_blocks[idx];

            if let Some(ref term) = data.terminator {
                self.worklist.extend(term.successors());
            }

            return Some((idx, data));
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // All the blocks, minus the number of blocks we've visited.
        let upper = self.body.basic_blocks.len() - self.visited.count();

        let lower = if self.root_is_start_block {
            // We will visit all remaining blocks exactly once.
            upper
        } else {
            self.worklist.len()
        };

        (lower, Some(upper))
    }
}

pub fn preorder<'a, 'tcx>(body: &'a Body<'tcx>) -> Preorder<'a, 'tcx> {
    Preorder::new(body, START_BLOCK)
}

pub fn reachable_as_bitset(body: &Body<'_>) -> BitSet<BasicBlock> {
    let mut iter = preorder(body);
    iter.by_ref().for_each(drop);
    iter.visited
}

pub struct Postorder<'a, 'tcx> {
    basic_blocks: &'a IndexSlice<BasicBlock, BasicBlockData<'tcx>>,
    visited: BitSet<BasicBlock>,
    visit_stack: Vec<(BasicBlock, Successors<'a>)>,
    root_is_start_block: bool,
}

impl<'a, 'tcx> Postorder<'a, 'tcx> {
    pub fn new(
        basic_blocks: &'a IndexSlice<BasicBlock, BasicBlockData<'tcx>>,
        root: BasicBlock,
    ) -> Postorder<'a, 'tcx> {
        let mut po = Postorder {
            basic_blocks,
            visited: BitSet::new_empty(basic_blocks.len()),
            visit_stack: Vec::new(),
            root_is_start_block: root == START_BLOCK,
        };

        let data = &po.basic_blocks[root];

        if let Some(ref term) = data.terminator {
            po.visited.insert(root);
            po.visit_stack.push((root, term.successors()));
            po.traverse_successor();
        }

        po
    }

    fn traverse_successor(&mut self) {
        while let Some(bb) = self.visit_stack.last_mut().and_then(|(_, iter)| iter.next_back()) {
            if self.visited.insert(bb) {
                if let Some(term) = &self.basic_blocks[bb].terminator {
                    self.visit_stack.push((bb, term.successors()));
                }
            }
        }
    }
}

impl<'tcx> Iterator for Postorder<'_, 'tcx> {
    type Item = BasicBlock;

    fn next(&mut self) -> Option<BasicBlock> {
        let (bb, _) = self.visit_stack.pop()?;
        self.traverse_successor();

        Some(bb)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let upper = self.basic_blocks.len() - self.visited.count();
        let lower = if self.root_is_start_block { upper } else { self.visit_stack.len() };

        (lower, Some(upper))
    }
}

pub fn reverse_postorder<'a, 'tcx>(
    body: &'a Body<'tcx>,
) -> impl Iterator<Item = (BasicBlock, &'a BasicBlockData<'tcx>)> + ExactSizeIterator + DoubleEndedIterator
{
    body.basic_blocks.reverse_postorder().into_iter().map(|bb| (bb, &body.basic_blocks[bb]))
}
