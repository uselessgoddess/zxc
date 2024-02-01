use std::{fmt, fmt::Write};

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

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Deref => f.write_str("*"),
            UnOp::Not => f.write_str("!"),
            UnOp::Neg => f.write_str("-"),
        }
    }
}
