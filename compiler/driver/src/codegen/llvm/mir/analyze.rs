use {
    crate::codegen::llvm::FunctionCx,
    middle::{
        mir::{Local, Rvalue, Statement},
        BitSet,
    },
};

pub fn non_ssa(fx: &FunctionCx<'_, '_, '_>) -> BitSet<Local> {
    let mut non_ssa = BitSet::new_empty(fx.mir.local_decls.len());

    return BitSet::new_filled(non_ssa.domain_size());

    // Simple non-ssa probe
    for bb in fx.mir.basic_blocks.iter() {
        for stmt in bb.statements.iter() {
            if let Statement::Assign(_, rvalue) = stmt
                && let Rvalue::Ref(_, place) = rvalue
            {
                non_ssa.insert(place.local);
            }
        }
    }

    non_ssa
}
