//! This module contains `TyKind`'s major components.

use {
    crate::codegen::mir::{
        ty::{self, List},
        Ty,
    },
    std::{fmt, fmt::Formatter},
};

#[derive(PartialEq, Eq, PartialOrd, Hash, Clone, Copy, Debug)]
#[non_exhaustive]
pub enum Abi {
    Zxc,
    C,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FnSig<'tcx> {
    pub inputs_and_output: &'tcx List<Ty<'tcx>>,
    pub abi: Abi,
}

impl<'tcx> FnSig<'tcx> {
    pub fn inputs(&self) -> &'tcx [Ty<'tcx>] {
        &self.inputs_and_output[..self.inputs_and_output.len() - 1]
    }

    pub fn output(&self) -> Ty<'tcx> {
        self.inputs_and_output[self.inputs_and_output.len() - 1]
    }
}

impl fmt::Debug for FnSig<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.abi {
            Abi::Zxc => (),
            abi => write!(f, "extern \"{abi:?}\"")?,
        };

        let inputs = self.inputs();
        write!(f, "fn(")?;
        if inputs.is_empty() {
            write!(f, ")")?
        } else {
            let (last, inputs) = inputs.split_last().unwrap();
            for ty in inputs {
                write!(f, "{ty}, ")?;
            }
            write!(f, "{last})")?;
        }

        match self.output().kind() {
            ty::Tuple(list) if list.is_empty() => Ok(()),
            _ => write!(f, " -> {:?}", &self.output()),
        }
    }
}
