use crate::{
    graph::{DirectedGraph, WithNumNodes, WithSuccessors},
    BitSet,
};

/// A "depth-first search" iterator for a directed graph.
pub struct DepthFirstSearch<'graph, G>
where
    G: ?Sized + DirectedGraph + WithNumNodes + WithSuccessors,
{
    graph: &'graph G,
    stack: Vec<G::Node>,
    visited: BitSet<G::Node>,
}

impl<'graph, G> DepthFirstSearch<'graph, G>
where
    G: ?Sized + DirectedGraph + WithNumNodes + WithSuccessors,
{
    pub fn new(graph: &'graph G) -> Self {
        Self { graph, stack: vec![], visited: BitSet::new_empty(graph.num_nodes()) }
    }

    pub fn with_start_node(mut self, start_node: G::Node) -> Self {
        self.push_start_node(start_node);
        self
    }

    pub fn push_start_node(&mut self, start_node: G::Node) {
        if self.visited.insert(start_node) {
            self.stack.push(start_node);
        }
    }
}

impl<G> Iterator for DepthFirstSearch<'_, G>
where
    G: ?Sized + DirectedGraph + WithNumNodes + WithSuccessors,
{
    type Item = G::Node;

    fn next(&mut self) -> Option<G::Node> {
        let DepthFirstSearch { stack, visited, graph } = self;
        let n = stack.pop()?;
        stack.extend(graph.successors(n).filter(|&m| visited.insert(m)));
        Some(n)
    }
}
