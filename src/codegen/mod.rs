use {
    crate::{
        lexer::{Lit, LitInt},
        parse::{BinOp, Binary, Expr, Paren, Stmt, UnOp, Unary},
        Span,
    },
    std::{
        error,
        fmt::{self, Formatter},
    },
};

pub struct GlobalCtx<'src> {
    scope: Scope<'src>,
}

struct Scope<'src> {
    stmts: Vec<Stmt<'src>>,
}

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicTypeEnum, IntType, VoidType},
    values::{AnyValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

type UnitType<'cx> = VoidType<'cx>;
type AllKind<'cx> = AnyTypeEnum<'cx>;
type AnyKind<'cx> = BasicTypeEnum<'cx>;

type AllValue<'cx> = AnyValueEnum<'cx>;
type AnyValue<'cx> = BasicValueEnum<'cx>;
type PtrValue<'cx> = PointerValue<'cx>;
type FnValue<'cx> = FunctionValue<'cx>;

pub enum Int {
    I64,
}

impl Int {
    fn must_be(self, any: AnyValue, span: Span) -> Result<IntValue> {
        if let AnyValue::IntValue(val) = any {
            Ok(val)
        } else {
            Err(Error::new(span, "expected `i64`"))
        }
    }
}

pub enum Ty {
    Unit,
    Int(Int),
}

struct TyCache<'cx> {
    pub unit: UnitType<'cx>,
    pub i64: IntType<'cx>,
}

struct Cache<'cx> {
    ty: TyCache<'cx>,
}

impl<'cx> Cache<'cx> {
    pub fn from_cx(cx: &'cx Context) -> Self {
        Cache { ty: TyCache { unit: cx.void_type(), i64: cx.i64_type() } }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub repr: String,
    pub span: Span,
}

impl Error {
    pub fn new(span: Span, repr: impl fmt::Display) -> Self {
        Self { repr: repr.to_string(), span }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}

impl error::Error for Error {}

type Result<T> = std::result::Result<T, Error>;

pub struct LLVM<'a, 'cx> {
    pub cx: &'cx Context,
    pub builder: &'a Builder<'cx>,
    pub module: &'a Module<'cx>,

    cache: Cache<'cx>,
}

use crate::parse::{Block, Local};

impl<'a, 'cx> LLVM<'a, 'cx> {
    fn new(cx: &'cx Context, builder: &'a Builder<'cx>, module: &'a Module<'cx>) -> Self {
        Self { cx, builder, module, cache: Cache::from_cx(cx) }
    }

    fn compile_fn(&mut self, name: &str, block: &[Stmt]) -> Result<FnValue<'cx>> {
        let ty = self.cache.ty.unit.fn_type(&[], false);
        let me = self.module.add_function(name, ty, None);

        self.builder.position_at_end(self.cx.append_basic_block(me, "entry"));

        if let Some((last, stmts)) = block.split_last() {
            for stmt in stmts {
                match stmt {
                    Stmt::Local(local) => self.compile_local(me, local)?,
                    Stmt::Expr(expr, semi) => {
                        assert!(semi.is_some());
                        let _ = self.compile_expr(expr)?;
                    }
                }
            }

            let ret = match last {
                Stmt::Local(local) => {
                    self.compile_local(me, local)?;
                    None
                }
                Stmt::Expr(expr, semi) => {
                    let expr = self.compile_expr(expr)?;
                    if semi.is_none() { Some(expr) } else { None }
                }
            };

            if let Some(ret) = ret {
                self.builder.build_return(Some(&ret));
            } else {
                self.builder.build_return(None);
            }
        } else {
            // assert ret type is ret type
            self.builder.build_return(None);
        }

        Ok(me)
    }

    fn alloca_in(&self, in_fn: FnValue<'cx>, ty: AnyKind<'cx>, name: &str) -> PtrValue<'cx> {
        let entry = in_fn.get_first_basic_block().expect("whatever");
        let local = self.cx.create_builder();

        match entry.get_first_instruction() {
            Some(first_instr) => local.position_before(&first_instr),
            None => local.position_at_end(entry),
        }

        local.build_alloca(ty, name)
    }

    fn compile_local(
        &mut self,
        in_fn: FnValue<'cx>,
        Local { pat, expr, .. }: &Local,
    ) -> Result<()> {
        match self.compile_expr(expr)? {
            AnyValue::IntValue(val) => self
                .builder
                .build_store(self.alloca_in(in_fn, self.cache.ty.i64.into(), pat.ident()), val),
            _ => todo!(),
        };

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<AnyValue<'cx>> {
        let Self { cx, builder, cache: Cache { ty, .. }, .. } = self;

        Ok(match expr {
            Expr::Lit(Lit::Int(LitInt { lit, .. })) => {
                AnyValue::IntValue(ty.i64.const_int(*lit, false))
            }
            Expr::Paren(Paren { expr, .. }) => self.compile_expr(expr)?,
            Expr::Unary(Unary { op, expr }) => match op {
                UnOp::Not(_) => match self.compile_expr(expr)? {
                    AnyValue::IntValue(val) => {
                        AnyValue::IntValue(self.builder.build_not(val, "not"))
                    }
                    _ => todo!(),
                },
                UnOp::Neg(_) => match self.compile_expr(expr)? {
                    AnyValue::IntValue(val) => {
                        AnyValue::IntValue(self.builder.build_int_neg(val, "neg"))
                    }
                    _ => todo!(),
                },
            },
            Expr::Binary(Binary { left, op, right }) => {
                let lhs = Int::I64.must_be(self.compile_expr(left)?, Span::splat(0))?;
                let rhs = Int::I64.must_be(self.compile_expr(right)?, Span::splat(0))?;

                match op {
                    BinOp::Add(_) => {
                        AnyValue::IntValue(self.builder.build_int_add(lhs, rhs, "add"))
                    }
                    BinOp::Sub(_) => {
                        AnyValue::IntValue(self.builder.build_int_sub(lhs, rhs, "sub"))
                    }
                    BinOp::Mul(_) => {
                        AnyValue::IntValue(self.builder.build_int_mul(lhs, rhs, "mu;"))
                    }
                    BinOp::Div(_) => {
                        AnyValue::IntValue(self.builder.build_int_unsigned_div(lhs, rhs, "div"))
                    }
                }
            }
            _ => todo!(),
        })
    }
}

// impl Backend for LLVM {
//     type Codegen = ();
//
//     fn codegen(gcx: GlobalCtx) -> Self::Codegen {
//         use inkwell::{
//             context::Context,
//             targets::{
//                 CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
//                 TargetTriple,
//             },
//             OptimizationLevel,
//         };
//
//         let ctx = Context::create();
//         let builder = ctx.create_builder();
//         let module = ctx.create_module("prove");
//
//         let void = ctx.void_type();
//         {
//             let func = module.add_function("main", void.fn_type(&[], false), None);
//
//             builder.position_at_end(ctx.append_basic_block(func, "entry"));
//
//             {
//                 pub struct Compiler<'a, 'ctx> {
//                     pub context: &'ctx Context,
//                     pub builder: &'a Builder<'ctx>,
//                     function: FunctionValue<'ctx>,
//                 }
//
//                 impl<'a, 'ctx> Compiler<'a, 'ctx> {
//                     fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
//                         let builder = self.context.create_builder();
//
//                         let entry = self.function.get_first_basic_block().unwrap();
//
//                         match entry.get_first_instruction() {
//                             Some(first_instr) => builder.position_before(&first_instr),
//                             None => builder.position_at_end(entry),
//                         }
//
//                         builder.build_alloca(self.context.f64_type(), name)
//                     }
//
//                     fn visit_expr(&mut self, expr: &Expr) -> IntValue<'ctx> {
//                         match expr {
//                             Expr::Lit(Lit::Int(LitInt { lit, .. })) => {
//                                 self.context.i64_type().const_int(*lit, false)
//                             }
//                             Expr::Let(Let { pat, expr, .. }) => {
//                                 let alloca = self.create_entry_block_alloca(pat.ident());
//                                 self.builder.build_store(alloca, self.visit_expr(expr));
//
//                                 self.context.i64_type().const_int(0, false)
//                             }
//                             Expr::Paren(Paren { expr, .. }) => self.visit_expr(expr),
//                             Expr::Unary(Unary { op, expr }) => match op {
//                                 UnOp::Not(_) => {
//                                     self.builder.build_not(self.visit_expr(expr), "not")
//                                 }
//                                 UnOp::Neg(_) => {
//                                     self.builder.build_int_neg(self.visit_expr(expr), "neg")
//                                 }
//                             },
//                             Expr::Binary(Binary { left, op, right }) => {
//                                 let lhs = self.visit_expr(left);
//                                 let rhs = self.visit_expr(right);
//
//                                 match op {
//                                     BinOp::Add(_) => self.builder.build_int_add(lhs, rhs, "add"),
//                                     BinOp::Sub(_) => self.builder.build_int_sub(lhs, rhs, "sub"),
//                                     BinOp::Mul(_) => self.builder.build_int_mul(lhs, rhs, "mu;"),
//                                     BinOp::Div(_) => {
//                                         self.builder.build_int_unsigned_div(lhs, rhs, "div")
//                                     }
//                                 }
//                             }
//                             _ => todo!(),
//                         }
//                     }
//                 }
//
//                 let mut compiler = Compiler { context: &ctx, builder: &builder, function: func };
//                 for stmt in gcx.scope.stmts {
//                     let Stmt::Expr(expr, _) = stmt;
//                     let _ = compiler.visit_expr(&expr);
//                 }
//             }
//
//             let _ = builder.build_return(None);
//
//             func.print_to_stderr();
//         }
//
//         {
//             Target::initialize_x86(&InitializationConfig { ..Default::default() });
//
//             let triplet = TargetMachine::get_default_triple();
//             let target = Target::from_triple(&triplet).unwrap();
//
//             let machine = target
//                 .create_target_machine(
//                     &TargetTriple::create("x86_64-pc-windows-gnu"),
//                     "x86-64",
//                     "+avx2",
//                     OptimizationLevel::None,
//                     RelocMode::Default,
//                     CodeModel::Small,
//                 )
//                 .unwrap();
//
//             machine.write_to_file(&module, FileType::Assembly, "a.out".as_ref()).unwrap();
//         }
//     }
// }

#[test]
fn codegen() {
    use {
        crate::{
            lexer::lexer,
            parse::{Block, ParseBuffer},
        },
        chumsky::Parser,
    };

    let src = r#"
        {
            let x = 228 * 1337;
            let y = 177013 - -1;
            12 + 342
        }
    "#;
    let mut input = ParseBuffer::new(lexer().parse(src).into_result().unwrap());
    let Block { stmts, .. } = input.parse().unwrap();

    println!("{stmts:#?}");

    // LLVM::codegen(GlobalCtx { scope: Scope { stmts } })

    let context = Context::create();
    let module = context.create_module("test_module");
    let builder = context.create_builder();

    let mut llvm = LLVM::new(&context, &builder, &module);

    let foo = llvm.compile_fn("main_fn", &stmts).unwrap();

    foo.print_to_stderr();
}

#[test]
fn foo() {
    let context = Context::create();
    let ty = context.i128_type();

    println!("{}", ty.get_bit_width());
}
