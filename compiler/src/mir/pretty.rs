use {
    crate::{
        hir::HirCtx,
        mir::{
            self,
            consts::ConstInt,
            ty::{self, Abi},
            ConstValue, Mutability, Operand, Rvalue, Statement, Terminator,
            TyKind::{self},
        },
    },
    lexer::{BinOp, UnOp},
    std::fmt::{self, Write as _},
};

macro_rules! p {
    (@$lit:literal) => {
        write!(scoped_cx!(), $lit)?
    };
    (@write($($data:expr),+)) => {
        write!(scoped_cx!(), $($data),+)?
    };
    (@print($x:expr)) => {
        $x.print(scoped_cx!())?
    };
    (@$method:ident($($arg:expr),*)) => {
        scoped_cx!().$method($($arg),*)?
    };
    ($($elem:tt $(($($args:tt)*))?),+) => {{
        $(p!(@ $elem $(($($args)*))?);)+
    }};
}
macro_rules! define_scoped_cx {
    ($cx:ident) => {
        macro_rules! scoped_cx {
            () => {
                $cx
            };
        }
    };
}

// TODO: add print macro
pub trait Printer<'tcx>: fmt::Write + Sized {
    fn hix<'a>(&'a self) -> &'a HirCtx<'tcx>;

    fn print_type(&mut self, ty: mir::Ty<'tcx>) -> fmt::Result;

    fn print_fn_sig(&mut self, inputs: &[mir::Ty<'tcx>], output: mir::Ty<'tcx>) -> fmt::Result {
        define_scoped_cx!(self);

        p!("(", comma_sep(inputs.iter().copied()), ")");
        if !output.is_unit() {
            p!(" -> ", print(output));
        }

        Ok(())
    }

    fn print_def_path(&mut self, def: mir::DefId) -> fmt::Result {
        let name = self.hix().instances[def].symbol;
        write!(self, "{name}")
    }

    fn print_const(&mut self, ct: ConstValue, ty: mir::Ty<'tcx>) -> fmt::Result {
        match (ct, ty.kind()) {
            (ConstValue::Scalar(scalar), _) => return self.print_const_scalar(scalar, ty),
            (ConstValue::Zst, ty::FnDef(def)) => return self.print_def_path(def),
            _ => {}
        }
        write!(self, "{ct:?}: ")?;
        ty.print(self)
    }

    fn print_const_scalar(&mut self, int: mir::ScalarRepr, ty: mir::Ty<'tcx>) -> fmt::Result {
        define_scoped_cx!(self);

        match ty.kind() {
            TyKind::Int(_) => {
                let int =
                    ConstInt::new(int, matches!(ty.kind(), ty::Int(_)), ty.is_ptr_sized_int());
                p!(write("{:#?}", int))
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn comma_sep<T>(&mut self, elems: impl Iterator<Item = T>) -> fmt::Result
    where
        T: Print<'tcx>,
    {
        self.comma_sep_with(elems, |cx, x| x.print(cx))
    }

    fn comma_sep_with<T>(
        &mut self,
        mut elems: impl Iterator<Item = T>,
        mut with: impl FnMut(&mut Self, T) -> fmt::Result,
    ) -> fmt::Result {
        if let Some(first) = elems.next() {
            with(self, first)?;
            for elem in elems {
                self.write_str(", ")?;
                with(self, elem)?;
            }
        }
        Ok(())
    }
}

pub trait Print<'tcx> {
    fn print<P: Printer<'tcx>>(&self, cx: &mut P) -> fmt::Result;
}

impl<'tcx> Print<'tcx> for mir::Ty<'tcx> {
    fn print<P: Printer<'tcx>>(&self, cx: &mut P) -> fmt::Result {
        cx.print_type(*self)
    }
}

pub trait DisplayCtx<'tcx> {
    fn to_string(&self, hix: &HirCtx<'tcx>) -> String;
}

impl<'tcx, All: Print<'tcx>> DisplayCtx<'tcx> for All {
    fn to_string(&self, hix: &HirCtx<'tcx>) -> String {
        let mut fmt = FmtPrinter::new(hix);
        self.print(&mut fmt).expect("an error occurred when formatting");
        fmt.into_buf()
    }
}

macro_rules! forward_display_to_print {
    ($($ty:ty),+) => {
        // TODO: wait lifters or more complex
        //$(#[allow(unused_lifetimes)] impl<'tcx> fmt::Display for $ty {
        //    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //        sess::with(|hix| {
        //            let mut cx = FmtPrinter::new(hix);
        //            self
        //                .print(&mut cx)?;
        //            f.write_str(&cx.into_buf())?;
        //            Ok(())
        //        })
        //    }
        //})+
    };
}

macro_rules! define_print {
    (($self:ident, $cx:ident): $($ty:ty $print:block)+) => {
        $(impl<'tcx> Print<'tcx> for $ty {
            fn print<P: Printer<'tcx>>(&$self, $cx: &mut P) -> fmt::Result {
                define_scoped_cx!($cx);
                let _: () = $print;
                Ok(())
            }
        })+
    };
}

macro_rules! define_print_and_forward_display {
    (($self:ident, $cx:ident): $($ty:ty $print:block)+) => {
        define_print!(($self, $cx): $($ty $print)*);
        forward_display_to_print!($($ty),+);
    };
}

define_print_and_forward_display! {
    (self, cx):

    mir::FnSig<'tcx> {
        if self.abi != Abi::Zxc {
            p!(write("extern {} ", self.abi));
        }

        p!("fn", print_fn_sig(self.inputs(), self.output()));
    }
}

pub struct FmtPrinter<'a, 'tcx> {
    pub hix: &'a HirCtx<'tcx>,
    fmt: String,
}

impl<'a, 'tcx> FmtPrinter<'a, 'tcx> {
    pub fn new(hix: &'a HirCtx<'tcx>) -> Self {
        Self { hix, fmt: String::new() }
    }

    pub fn into_buf(self) -> String {
        self.fmt
    }
}

impl fmt::Write for FmtPrinter<'_, '_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        Ok(self.fmt.push_str(s))
    }
}

impl<'tcx> Printer<'tcx> for FmtPrinter<'_, 'tcx> {
    fn hix<'a>(&'a self) -> &'a HirCtx<'tcx> {
        self.hix
    }

    fn print_type(&mut self, ty: mir::Ty<'tcx>) -> fmt::Result {
        define_scoped_cx!(self);

        match ty.kind() {
            ty::Int(int) => p!(write("{}", int.name_str())),
            ty::Tuple(list) => {
                if list.is_empty() {
                    p!("@unit")
                } else {
                    p!("(", comma_sep(list.iter()));
                    if list.len() == 1 {
                        p!(",");
                    }
                    p!(")")
                }
            }
            ty::FnDef(def) => {
                let sig = self.hix.instances[def].sig;
                p!(print(sig))
            }
        }

        Ok(())
    }
}

define_print_and_forward_display! {
    (self, cx):

    mir::Place<'tcx> {
        p!(write("_{}", self.local.raw()));
    }

    Operand<'tcx> {
        match *self {
            Operand::Copy(place) => {
                p!(print(place))
            }
            Operand::Const(const_, ty) => {
                match ty.kind() {
                    ty::FnDef(_) => {}
                    _ => p!(write("const ")),
                }
                p!(print_const(const_, ty))
            }
        }
    }

    Terminator<'tcx> {
        match self {
            Terminator::Goto { target } => {
                p!(write("goto -> bb{}", target.raw()));
            }
            Terminator::Return => p!("return"),
            Terminator::Unreachable => p!("unreachable"),
            Terminator::Call { func, args, dest, target, .. } => {
                p!(print(dest), " = ");
                p!(print(func), "(");
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        p!(", ");
                    }
                    p!(print(arg));
                }
                p!(")");
                if let Some(target) = target {
                    p!(write(" -> [return: bb{}]", target.raw()))
                }
            }
        }
    }

    Rvalue<'tcx> {
        match self {
            Rvalue::Use(operand) => p!(print(operand)),
            Rvalue::UseDeref(operand) =>p!("*", print(operand)),
            Rvalue::UnaryOp(op, operand) => {
                let op = match op {
                    UnOp::Not(_) => "Not",
                    UnOp::Neg(_) => "Neg",
                };
                p!(write("{op}"), "(", print(operand), ")")
            }
            Rvalue::BinaryOp(op, lhs, rhs) => {
                let op = match op {
                    BinOp::Add(_) => "Add",
                    BinOp::Sub(_) => "Sub",
                    BinOp::Mul(_) => "Mul",
                    BinOp::Div(_) => "Div",
                };
                p!(write("{op}"), "(", print(lhs), ", ", print(rhs), ")")
            }
            Rvalue::Cast(kind, from, cast) => {
                p!(print(from), " as ", print(cast), write(" ({kind:?})"))
            }
        }
    }

    Statement<'tcx> {
        match self {
            Statement::Assign(place, rvalue) => p!(print(place), " = ", print(rvalue)),
            Statement::Nop => p!("Nop"),
        }
    }
}

pub fn write_mir_body_pretty<'tcx>(
    body: &mir::Body<'tcx>,
    in_fn: bool,
    w: &mut impl Printer<'tcx>,
) -> fmt::Result {
    define_scoped_cx!(w);

    let tab = if in_fn { "    " } else { "" };

    let ret = Some(mir::Local::RETURN_PLACE).into_iter();
    for local in ret.chain(body.vars_and_temps_iter()) {
        let decl = body.local_decls[local];
        let m = match decl.mutability {
            Mutability::Not => " ",
            Mutability::Mut => " mut ",
        };
        p!(write("{tab}let{m}_{}: ", local.raw()), print(decl.ty), ";\n");
    }
    p!("\n");

    for (bb, block) in body.basic_blocks.iter_enumerated() {
        writeln!(w, "{tab}bb{}: {{", bb.raw())?;

        for stmt in &block.statements {
            p!("    {tab}", print(stmt), ";");
            writeln!(w)?;
        }

        let terminator = block.terminator();
        p!("    {tab}", print(terminator), ";");
        writeln!(w, "\n{tab}}}")?;

        if bb != body.basic_blocks.len() - 1 {
            writeln!(w)?;
        }
    }

    Ok(())
}

pub fn write_mir_pretty<'tcx>(
    mir: mir::DefId,
    body: &mir::Body<'tcx>,
    w: &mut impl Printer<'tcx>,
) -> fmt::Result {
    define_scoped_cx!(w);

    let mir::InstanceData { sig, hsig, .. } = w.hix().instances[mir];

    if sig.abi != Abi::Zxc {
        p!(write("extern \"{:?}\" ", sig.abi));
    }
    p!(write("fn {}", hsig.decl.name));

    p!("(");
    w.comma_sep_with(sig.inputs().iter().enumerate(), |cx, (local, ty)| {
        write!(cx, "_{}: ", local + 1)?;
        ty.print(cx)
    })?;
    p!(")");
    p!(" -> ", print(sig.output()));

    p!(" {{\n");
    write_mir_body_pretty(body, true, w)?;
    p!("}}\n\n");

    Ok(())
}
