mod arenas;
mod ctx;
mod intern;
mod list;
mod ty;

pub use {
    arenas::{DroplessArena, TypedArena},
    ctx::{Arena, Local, Session, Tx, TyCtx},
    intern::{InternSet, Interned},
    ty::{IntTy, Ty, TyKind},
};
use {
    cranelift::{
        codegen::{
            ir::{immediates::Offset32, Function, StackSlot, UserFuncName},
            Context,
        },
        prelude::{
            isa, settings, types, AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder,
            MemFlags, Signature, Value, Variable,
        },
    },
    cranelift_module::{FuncId, Linkage, Module},
    std::{collections::HashMap, fs, io, mem, path::PathBuf, process::Command},
    target_lexicon::Triple,
};

mod abi {
    use {crate::codegen::Interned, index_vec::IndexVec};

    index_vec::define_index_type! {
        pub struct FieldIdx = u32;
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub enum FieldsShape {
        Primitive,
        Arbitrary { offsets: IndexVec<FieldIdx, Size>, memory_index: IndexVec<FieldIdx, u32> },
    }

    impl FieldsShape {
        pub fn count(&self) -> usize {
            match *self {
                FieldsShape::Primitive => 0,
                FieldsShape::Arbitrary { ref offsets, .. } => offsets.len(),
            }
        }

        pub fn offset(&self, i: usize) -> Size {
            match *self {
                FieldsShape::Primitive => {
                    unreachable!("FieldsShape::offset: `Primitive`s have no fields")
                }
                FieldsShape::Arbitrary { ref offsets, .. } => offsets[FieldIdx::from_usize(i)],
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub struct Align {
        pow2: u8,
    }

    impl Align {
        pub const ONE: Align = Align { pow2: 0 };
        pub const MAX: Align = Align { pow2: 29 };

        pub fn from_bytes(align: u64) -> Option<Self> {
            if align == 0 {
                return Some(Align::ONE);
            }

            let tz = align.trailing_zeros();
            if align != (1 << tz) {
                return None; // not power of two
            }

            let pow2 = tz as u8;
            if pow2 > Self::MAX.pow2 {
                return None; // too large
            }

            Some(Align { pow2 })
        }

        pub fn bytes(self) -> u64 {
            1 << self.pow2
        }

        pub fn bits(self) -> u64 {
            self.bytes() * 8
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub struct Size {
        raw: u64,
    }

    impl Size {
        pub const ZERO: Size = Size { raw: 0 };

        pub fn from_bytes(bytes: u64) -> Self {
            Self { raw: bytes }
        }

        pub fn bytes(self) -> u64 {
            self.raw
        }

        pub fn is_aligned(self, align: Align) -> bool {
            let mask = align.bytes() - 1;
            self.bytes() & mask == 0
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum Integer {
        I8,
        I16,
        I32,
        I64,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum Scalar {
        Int(Integer, bool),
        F32,
        F64,
        Pointer,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum Abi {
        Scalar(Scalar),
        Aggregate,
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub struct LayoutKind {
        pub abi: Abi,
        pub size: Size,
        pub align: Align,
        pub shape: FieldsShape,
    }

    pub type Layout<'cx> = Interned<'cx, LayoutKind>;

    #[derive(Debug, Copy, Clone)]
    pub enum PassMode {
        Ignore,
        Direct, // Add ArgAttributes
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct TyAbi<'tcx> {
        pub ty: super::Ty<'tcx>,
        pub layout: Layout<'tcx>,
    }

    #[derive(Debug, Copy, Clone)]
    pub struct ArgAbi<'tcx> {
        pub ty: TyAbi<'tcx>,
        pub mode: PassMode,
    }
}

use crate::{
    codegen::abi::{Abi, ArgAbi, Integer, PassMode, Scalar, TyAbi},
    lexer::{Ident, Lit},
    parse::{self, BinOp, Spanned, UnOp},
    Span,
};

#[derive(Debug, Clone)]
pub enum ExprKind<'tcx> {
    Lit(Lit<'tcx>),
    Var(&'tcx str),
    Unary(UnOp, &'tcx Expr<'tcx>),
    Binary(BinOp, &'tcx Expr<'tcx>, &'tcx Expr<'tcx>),
}

#[derive(Debug, Clone)]
pub struct Expr<'tcx> {
    pub kind: ExprKind<'tcx>,
    pub span: Span,
}

impl<'tcx> Expr<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, expr: &parse::Expr<'tcx>) -> &'tcx Self {
        let span = expr.span();

        use crate::parse::expr::{Binary, Paren, Unary};
        let kind = match expr {
            parse::Expr::Lit(lit) => expr::Lit(*lit),
            parse::Expr::Paren(Paren { expr, .. }) => return Self::analyze(tcx, expr),
            parse::Expr::Unary(Unary { op, expr }) => expr::Unary(*op, Self::analyze(tcx, expr)),
            parse::Expr::Binary(Binary { left, op, right }) => {
                expr::Binary(*op, Self::analyze(tcx, left), Self::analyze(tcx, right))
            }
            parse::Expr::Ident(str) => expr::Var(str.ident()),
        };
        tcx.arena.expr.alloc(Self { kind, span })
    }
}

#[derive(Debug, Clone)]
pub struct LocalStmt<'tcx> {
    pub pat: Ident<'tcx>,
    pub init: &'tcx Expr<'tcx>,
}

#[derive(Debug, Clone)]
pub enum StmtKind<'tcx> {
    Local(LocalStmt<'tcx>),
    Expr(&'tcx Expr<'tcx>, /* semi */ bool),
}

#[derive(Debug, Clone)]
pub struct Stmt<'tcx> {
    pub kind: StmtKind<'tcx>,
    pub span: Span,
}

impl<'tcx> Stmt<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, stmt: parse::Stmt<'tcx>) -> Self {
        let span = stmt.span();
        let kind = match stmt {
            parse::Stmt::Local(local) => {
                StmtKind::Local(LocalStmt { pat: local.pat, init: Expr::analyze(tcx, &local.expr) })
            }
            parse::Stmt::Expr(expr, semi) => {
                StmtKind::Expr(Expr::analyze(tcx, &expr), semi.is_none())
            }
        };
        Self { kind, span }
    }
}

pub mod stmt {
    pub use super::StmtKind::*;
}

pub mod expr {
    pub use super::ExprKind::*;
}

pub struct FnAbi<'tcx> {
    pub symbol: String,
    pub args: Box<[ArgAbi<'tcx>]>,
    pub ret: ArgAbi<'tcx>,
}

pub struct Instance<'tcx> {
    pub abi: FnAbi<'tcx>,
    pub body: Box<[Stmt<'tcx>]>,
}

#[derive(Debug, Copy, Clone)] // FIXME: want be `Clone`
pub(crate) struct CValue<'tcx> {
    inner: CValueInner,
    layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
enum CValueInner {
    ByRef(Pointer),
    ByVal(Value),
}

impl<'tcx> CValue<'tcx> {
    pub fn layout(&self) -> TyAbi<'tcx> {
        self.layout
    }

    pub fn by_val(value: Value, layout: TyAbi<'tcx>) -> CValue<'tcx> {
        CValue { inner: CValueInner::ByVal(value), layout }
    }

    pub fn by_ref(ptr: Pointer, layout: TyAbi<'tcx>) -> CValue<'tcx> {
        CValue { inner: CValueInner::ByRef(ptr), layout }
    }

    pub fn load_scalar(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> Value {
        let layout = self.layout();
        match self.inner {
            CValueInner::ByRef(ptr) => {
                let clif_ty = match layout.layout.abi {
                    Abi::Scalar(scalar) => scalar_to_clif(fx.tcx, scalar),
                    _ => unreachable!("{:?}", layout.ty),
                };
                ptr.load(fx, clif_ty, MemFlags::new().with_notrap())
            }
            CValueInner::ByVal(value) => value,
        }
    }
}

#[derive(Debug, Copy, Clone)] // FIXME: want be `Clone`
pub(crate) struct CPlace<'tcx> {
    inner: CPlaceInner,
    layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
enum CPlaceInner {
    Var(Local, Variable),
    Addr(Pointer),
}

impl<'tcx> CPlace<'tcx> {
    pub fn layout(&self) -> TyAbi<'tcx> {
        self.layout
    }

    pub fn new_var(
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        local: Local,
        layout: TyAbi<'tcx>,
    ) -> CPlace<'tcx> {
        let var = Variable::from_u32(fx.next_ssa());
        fx.bcx.declare_var(var, fx.clif_type(layout.ty).expect("LMAO"));
        CPlace { inner: CPlaceInner::Var(local, var), layout }
    }

    pub(crate) fn into_value(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> CValue<'tcx> {
        let layout = self.layout();
        match self.inner {
            CPlaceInner::Var(_local, var) => {
                let val = fx.bcx.use_var(var);
                CValue::by_val(val, layout)
            }
            CPlaceInner::Addr(ptr) => CValue::by_ref(ptr, layout),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Pointer {
    base: PointerBase,
    offset: Offset32,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum PointerBase {
    Addr(Value),
    Stack(StackSlot),
}

impl Pointer {
    pub fn new(addr: Value) -> Self {
        Pointer { base: PointerBase::Addr(addr), offset: Offset32::new(0) }
    }

    pub fn stack_slot(stack_slot: StackSlot) -> Self {
        Pointer { base: PointerBase::Stack(stack_slot), offset: Offset32::new(0) }
    }

    pub fn load(self, fx: &mut FunctionCx<'_, '_, '_>, ty: types::Type, flags: MemFlags) -> Value {
        match self.base {
            PointerBase::Addr(base_addr) => fx.bcx.ins().load(ty, flags, base_addr, self.offset),
            PointerBase::Stack(stack_slot) => fx.bcx.ins().stack_load(ty, stack_slot, self.offset),
        }
    }

    pub fn store(self, fx: &mut FunctionCx<'_, '_, '_>, value: Value, flags: MemFlags) {
        match self.base {
            PointerBase::Addr(base_addr) => {
                fx.bcx.ins().store(flags, value, base_addr, self.offset);
            }
            PointerBase::Stack(stack_slot) => {
                fx.bcx.ins().stack_store(value, stack_slot, self.offset);
            }
        }
    }
}

struct Scope<'cl, 'tcx> {
    inner: HashMap<&'cl str, CPlace<'tcx>>,
}

impl<'cl, 'tcx> Scope<'cl, 'tcx> {
    pub fn new() -> Self {
        Self { inner: HashMap::new() }
    }

    pub fn enter(&self) -> Self {
        Self { inner: self.inner.clone() }
    }

    pub fn declare_var(
        &mut self,
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        layout: TyAbi<'tcx>,
        ident: &'cl str,
    ) -> CPlace<'tcx> {
        let ssa = Local::from_usize(fx.next_ssa() as usize);
        let var = CPlace::new_var(fx, ssa, layout);
        self.inner.insert(ident, var);
        var
    }

    pub fn use_var(
        &mut self,
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        layout: TyAbi<'tcx>,
        ident: &'cl str,
    ) -> CPlace<'tcx> {
        if let Some(var) = self.inner.get(ident) {
            *var
        } else {
            self.declare_var(fx, layout, ident)
        }
    }

    pub fn get_var(&mut self, ident: &str) -> Option<CPlace<'tcx>> {
        self.inner.get(ident).map(mem::copy)
    }
}

pub struct FunctionCx<'m, 'cl, 'tcx: 'm> {
    tcx: Tx<'tcx>,
    bcx: FunctionBuilder<'cl>,
    module: &'m mut dyn Module,
    abi: FnAbi<'tcx>,
    body: &'cl [Stmt<'tcx>],

    ptr: types::Type,
    next_ssa: u32,
}

impl<'tcx> FunctionCx<'_, '_, 'tcx> {
    pub fn next_ssa(&mut self) -> u32 {
        let ret = self.next_ssa;
        self.next_ssa += 1;
        ret
    }

    fn clif_type(&self, ty: Ty<'tcx>) -> Option<types::Type> {
        ty::clif_type_from_ty(self.tcx, ty)
    }
}

pub(crate) fn pointer_ty(tcx: Tx<'_>) -> types::Type {
    // match tcx.data_layout.pointer_size.bits() {
    //     16 => types::I16,
    //     32 => types::I32,
    //     64 => types::I64,
    //     bits => todo!("unknown bits: {bits}"),
    // }
    types::I64
}

pub(crate) fn scalar_to_clif(tcx: Tx<'_>, scalar: Scalar) -> types::Type {
    match scalar {
        Scalar::Int(int, _sign) => match int {
            Integer::I8 => types::I8,
            Integer::I16 => types::I16,
            Integer::I32 => types::I32,
            Integer::I64 => types::I64,
        },
        Scalar::F32 => types::F32,
        Scalar::F64 => types::F64,
        Scalar::Pointer => pointer_ty(tcx),
    }
}

fn sig_from_abi<'tcx>(tcx: Tx<'tcx>, abi: &FnAbi<'tcx>) -> Signature {
    let from_abi = |ty: &ArgAbi| match ty.mode {
        PassMode::Ignore => None,
        PassMode::Direct => Some(match ty.ty.layout.abi {
            Abi::Scalar(scalar) => AbiParam::new(scalar_to_clif(tcx, scalar)),
            Abi::Aggregate => todo!(),
        }),
    };

    let params = abi.args.iter().flat_map(from_abi).collect();
    let ret = from_abi(&abi.ret);

    Signature { params, returns: Vec::from_iter(ret), call_conv: isa::CallConv::SystemV }
}

fn codegen_expr<'cl, 'tcx>(
    fx: &mut FunctionCx<'_, 'cl, 'tcx>,
    scope: &mut Scope<'cl, 'tcx>,
    expr: &Expr<'tcx>,
) -> CValue<'tcx> {
    match &expr.kind {
        expr::Lit(Lit::Int(int)) => {
            let val = fx.bcx.ins().iconst(types::I64, int.lit as i64);
            CValue::by_val(val, fx.tcx.layout_of(fx.tcx.types.i64))
        }
        expr::Var(name) => scope.get_var(name).expect("compile error").into_value(fx),
        expr::Unary(op, expr) => match op {
            UnOp::Neg(_) => {
                let val = codegen_expr(fx, scope, expr);
                let scalar = val.load_scalar(fx);
                CValue::by_val(fx.bcx.ins().ineg(scalar), val.layout())
            }
            _ => todo!(),
        },
        expr::Binary(op, lhs, rhs) => match op {
            BinOp::Add(_) => {
                let lhs = codegen_expr(fx, scope, lhs);
                let rhs = codegen_expr(fx, scope, rhs);

                assert_eq!(lhs.layout(), rhs.layout());

                let a = lhs.load_scalar(fx);
                let b = rhs.load_scalar(fx);
                CValue::by_val(fx.bcx.ins().iadd(a, b), lhs.layout())
            }
            BinOp::Sub(_) => {
                let lhs = codegen_expr(fx, scope, lhs);
                let rhs = codegen_expr(fx, scope, rhs);

                assert_eq!(lhs.layout(), rhs.layout());

                let a = lhs.load_scalar(fx);
                let b = rhs.load_scalar(fx);
                CValue::by_val(fx.bcx.ins().isub(a, b), lhs.layout())
            }
            BinOp::Mul(_) => {
                let lhs = codegen_expr(fx, scope, lhs);
                let rhs = codegen_expr(fx, scope, rhs);

                assert_eq!(lhs.layout(), rhs.layout());

                let a = lhs.load_scalar(fx);
                let b = rhs.load_scalar(fx);
                CValue::by_val(fx.bcx.ins().imul(a, b), lhs.layout())
            }
            BinOp::Div(_) => {
                let lhs = codegen_expr(fx, scope, lhs);
                let rhs = codegen_expr(fx, scope, rhs);

                assert_eq!(lhs.layout(), rhs.layout());

                let a = lhs.load_scalar(fx);
                let b = rhs.load_scalar(fx);
                CValue::by_val(fx.bcx.ins().sdiv(a, b), lhs.layout())
            }
            _ => todo!(),
        },
        _ => todo!(),
    }
}

fn codegen_stmt<'cl, 'tcx>(
    fx: &mut FunctionCx<'_, 'cl, 'tcx>,
    scope: &mut Scope<'cl, 'tcx>,
    stmt: &'cl Stmt<'tcx>,
) {
    match &stmt.kind {
        stmt::Local(LocalStmt { pat, init }) => {
            let init = codegen_expr(fx, scope, init);
            let place = scope.declare_var(fx, init.layout(), pat.ident());
            // TODO: fix
            match place.inner {
                CPlaceInner::Var(local, var) => {
                    let val = init.load_scalar(fx);
                    fx.bcx.def_var(var, val);
                }
                _ => todo!(),
            }
        }
        stmt::Expr(_, _) => {}
    }
}

fn codegen_return<'tcx>(fx: &mut FunctionCx<'_, '_, 'tcx>, val: CValue<'tcx>) {
    match &fx.abi.ret.mode {
        PassMode::Ignore => {
            fx.bcx.ins().return_(&[]);
        }
        PassMode::Direct => {
            let ret = val.load_scalar(fx);
            fx.bcx.ins().return_(&[ret]);
        }
    }
}

fn codegen_fn_body(fx: &mut FunctionCx<'_, '_, '_>) {
    let mut scope = Scope::new();
    if let Some((tail, body)) = fx.body.split_last() {
        let block = fx.bcx.create_block();
        fx.bcx.switch_to_block(block);

        for stmt in body {
            if let stmt::Expr(_, true) = &stmt.kind {
                panic!()
            }
            codegen_stmt(fx, &mut scope, stmt);
        }

        if let stmt::Expr(expr, true) = &tail.kind {
            let val = codegen_expr(fx, &mut scope, expr);
            codegen_return(fx, val);
        } else {
            codegen_stmt(fx, &mut scope, tail);
            fx.bcx.ins().return_(&[]);
        }
    }
}

fn compile_fn<'tcx>(
    tcx: Tx<'tcx>,
    module: &mut dyn Module,
    Instance { abi, ref body }: Instance<'tcx>,
) -> (FuncId, Function) {
    let sig = sig_from_abi(tcx, &abi);
    let id =
        module.declare_function(&abi.symbol, Linkage::Export, &sig).unwrap_or_else(|_| todo!());

    let mut fn_ctx = FunctionBuilderContext::new();
    let mut fn_ = Function::with_name_signature(UserFuncName::user(0, id.as_u32()), sig);

    let mut bcx = FunctionBuilder::new(&mut fn_, &mut fn_ctx);

    let mut fx = FunctionCx { tcx, bcx, module, abi, body, ptr: Default::default(), next_ssa: 0 };

    codegen_fn_body(&mut fx);

    fx.bcx.seal_all_blocks();
    fx.bcx.finalize();
    (id, fn_)
}

fn linker_and_flavor(_: &Session) -> (PathBuf /* LinkerFlavor */,) {
    (PathBuf::from("gcc"),)
}

fn link_binary(sess: &Session, module: PathBuf, output: PathBuf) -> io::Result<()> {
    // now it's just one `module` and `output`

    let (linker,) = linker_and_flavor(sess);

    Command::new(linker).arg(module).arg("-o").arg(output).output().map(|_| ())
}

#[test]
fn codegen() -> Result<(), Box<dyn std::error::Error>> {
    use {
        crate::{
            lexer::lexer,
            parse::{Block, ParseBuffer},
        },
        chumsky::Parser,
    };

    let src = r#"
        {
            let a = 1;
            let b = -5;
            let c = 6;

            let d = b * b - 4 * a * c;
            (-b + d) / 2 * a
        }
    "#;

    let mut input =
        ParseBuffer::new(lexer().parse(src).into_result().unwrap_or_else(|vec| panic!("{vec:#?}")));
    let Block { stmts, .. } = input.parse()?;

    let module_path = "my_funny_module.o";
    let mut module = {
        use {cranelift_module as module, cranelift_object as object};

        let isa = isa::lookup(Triple::host())?.finish(settings::Flags::new(settings::builder()))?;
        let mut out =
            object::ObjectBuilder::new(isa, module_path, module::default_libcall_names())?;
        out.per_function_section(false);
        object::ObjectModule::new(out)
    };

    let tcx = TyCtx::enter(Session {});

    let i64 = tcx.types.i64;
    let (id, main) = compile_fn(
        &tcx,
        &mut module,
        Instance {
            abi: FnAbi {
                symbol: "main".to_string(),
                args: Box::new([]),
                // ret: ArgAbi { ty: tcx.layout_of(unit), mode: PassMode::Ignore },
                ret: ArgAbi { ty: tcx.layout_of(i64), mode: PassMode::Direct },
            },
            body: stmts.iter().map(|stmt| Stmt::analyze(&tcx, stmt.clone())).collect(),
        },
    );
    println!("{main}");
    module.define_function(id, &mut Context::for_function(main))?;

    let path = PathBuf::from(module_path);
    fs::write(&path, module.finish().emit()?)?;

    link_binary(&Session {}, path, "binary.out".into()).unwrap_or_else(|_| panic!("linker errors"));

    Ok(())
}
