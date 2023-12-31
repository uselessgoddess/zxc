use {
    crate::{
        codegen::{
            mir::{self, Operand, Rvalue, Statement, Terminator},
            Tx,
        },
        parse::{BinOp, UnOp},
    },
    std::{fmt, fmt::Formatter, io},
};

impl fmt::Debug for mir::Place<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        assert!(self.projection.is_empty());

        write!(f, "_{}", self.local.raw())
    }
}

impl fmt::Debug for Operand<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Copy(place) => {
                write!(f, "{place:?}")
            }
            Operand::Const(const_, ty) => {
                write!(f, "Const::<{ty}>({:?})", const_)
            }
        }
    }
}

impl fmt::Debug for Rvalue<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Rvalue::Use(operand) => write!(f, "{operand:?}"),
            Rvalue::UseDeref(operand) => write!(f, "*{operand:?}"),
            Rvalue::UnaryOp(op, operand) => write!(
                f,
                "{}({operand:?})",
                match op {
                    UnOp::Not(_) => "Not",
                    UnOp::Neg(_) => "Neg",
                }
            ),
            Rvalue::BinaryOp(op, lhs, rhs) => {
                write!(
                    f,
                    "{}({lhs:?}, {rhs:?})",
                    match op {
                        BinOp::Add(_) => "Add",
                        BinOp::Sub(_) => "Sub",
                        BinOp::Mul(_) => "Mul",
                        BinOp::Div(_) => "Div",
                    }
                )
            }
            Rvalue::Cast(kind, from, cast) => {
                write!(f, "{from:?} as {cast} ({kind:?})")
            }
        }
    }
}

impl fmt::Debug for Statement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Assign(place, rvalue) => write!(f, "{place:?} = {rvalue:?}"),
            Statement::Nop => write!(f, "Nop"),
        }
    }
}

impl fmt::Debug for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Goto { target } => {
                write!(f, "goto -> bb{}", target.raw())
            }
            Terminator::Return => write!(f, "return"),
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}

pub fn write_mir_body_pretty<'tcx>(
    _: Tx<'tcx>,
    body: &mir::Body<'tcx>,
    w: &mut dyn io::Write,
) -> io::Result<()> {
    for local in body.args_iter() {
        writeln!(w, "let _{}: {}", local.raw(), body.local_decls[local].ty)?;
    }

    for (bb, block) in body.basic_blocks.iter_enumerated() {
        writeln!(w, "bb{}: {{", bb.raw())?;

        for stmt in &block.statements {
            write!(w, "    {stmt:?};")?;
            writeln!(w)?;
        }

        writeln!(w, "    {:?};", block.terminator())?;
        writeln!(w, "}}")?;
        writeln!(w)?;
    }

    Ok(())
}
