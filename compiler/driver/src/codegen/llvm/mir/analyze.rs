use {
    crate::codegen::llvm::FunctionCx,
    middle::{
        mir::{Local, Rvalue, StatementKind},
        BitSet,
    },
};

pub fn non_ssa(fx: &FunctionCx<'_, '_, '_>) -> BitSet<Local> {
    BitSet::new_filled(fx.mir.local_decls.len())
}
