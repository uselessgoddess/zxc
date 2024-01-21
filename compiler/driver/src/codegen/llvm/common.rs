use {crate::codegen::ssa, llvm::TypeKind};

impl From<llvm::TypeKind> for ssa::TypeKind {
    fn from(value: TypeKind) -> Self {
        match value {
            TypeKind::Void => ssa::TypeKind::Void,
            TypeKind::Half => ssa::TypeKind::Half,
            TypeKind::Float => ssa::TypeKind::Float,
            TypeKind::Double => ssa::TypeKind::Double,
            TypeKind::X86_FP80 => ssa::TypeKind::X86_FP80,
            TypeKind::FP128 => ssa::TypeKind::FP128,
            TypeKind::PPC_FP128 => ssa::TypeKind::PPC_FP128,
            TypeKind::Label => ssa::TypeKind::Label,
            TypeKind::Integer => ssa::TypeKind::Integer,
            TypeKind::Function => ssa::TypeKind::Function,
            TypeKind::Struct => ssa::TypeKind::Struct,
            TypeKind::Array => ssa::TypeKind::Array,
            TypeKind::Pointer => ssa::TypeKind::Pointer,
            TypeKind::Vector => ssa::TypeKind::Vector,
            TypeKind::Metadata => ssa::TypeKind::Metadata,
            TypeKind::X86_MMX => ssa::TypeKind::X86_MMX,
            TypeKind::Token => ssa::TypeKind::Token,
            TypeKind::ScalableVector => ssa::TypeKind::ScalableVector,
            TypeKind::BFloat => ssa::TypeKind::BFloat,
            TypeKind::X86_AMX => ssa::TypeKind::X86_AMX,
        }
    }
}
