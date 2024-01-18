#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnOp {
    /// The `*` operator (dereferencing).
    Deref,
    /// The `!` operator (logical negation).
    Not,
    /// The `-` operator (negation).
    Neg,
}

impl UnOp {
    pub fn from_parse(op: lexer::UnOp) -> Self {
        match op {
            lexer::UnOp::Deref(_) => Self::Deref,
            lexer::UnOp::Not(_) => Self::Not,
            lexer::UnOp::Neg(_) => Self::Neg,
        }
    }
}
