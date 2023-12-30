use crate::{
    codegen::{
        mir,
        mir::{Operand, Rvalue, Statement, Terminator},
    },
    parse::{BinOp, UnOp},
};

pub fn mir_place(place: &mir::Place) {
    assert!(place.projection.is_empty());

    print!("_{}", place.local.raw());
}

pub fn mir_operand(operand: &Operand) {
    match operand {
        Operand::Copy(place) => {
            mir_place(place);
        }
        Operand::Const(const_, ty) => {
            print!("Const::<{ty}>({:?})", const_);
        }
    }
}

pub fn mir_rvalue(rvalue: &Rvalue) {
    match rvalue {
        Rvalue::Use(operand) => mir_operand(operand),
        Rvalue::UseDeref(operand) => {
            print!("*");
            mir_place(operand);
        }
        Rvalue::UnaryOp(op, operand) => {
            print!(
                "{}(",
                match op {
                    UnOp::Not(_) => "Not",
                    UnOp::Neg(_) => "Neg",
                }
            );
            mir_operand(operand);
            print!(")");
        }
        Rvalue::BinaryOp(op, lhs, rhs) => {
            print!(
                "{}(",
                match op {
                    BinOp::Add(_) => "UncheckedAdd",
                    BinOp::Sub(_) => "UncheckedSub",
                    BinOp::Mul(_) => "UncheckedMul",
                    BinOp::Div(_) => "UncheckedDiv",
                }
            );
            mir_operand(lhs);
            print!(", ");
            mir_operand(rhs);
            print!(")");
        }
    }
}

pub fn mir_stmt(stmt: &Statement) {
    match stmt {
        Statement::Assign(place, rvalue) => {
            mir_place(place);
            print!(" = ");
            mir_rvalue(rvalue);
        }
        Statement::Nop => {
            print!("Nop")
        }
    }
}

pub fn mir_body(mir: &mir::Body) {
    for (bb, block) in mir.basic_blocks.iter_enumerated() {
        println!("bb{}: {{", bb.raw());

        for stmt in &block.statements {
            print!("    ");
            mir_stmt(stmt);
            println!();
        }

        print!("    ");
        match block.terminator() {
            Terminator::Goto { target } => {
                print!("goto -> bb{}", target.raw());
            }
            Terminator::Return => {
                print!("return");
            }
            Terminator::Unreachable => {
                print!("unreachable");
            }
        }

        println!("\n}}");
        println!();
    }
}
