use {
    crate::{
        lexer::{Lit, LitInt},
        parse::{BinOp, Binary, Expr, Let, Paren, Stmt, UnOp, Unary},
    },
    inkwell::{
        builder::Builder,
        values::{FunctionValue, IntValue, PointerValue},
    },
};

pub struct GlobalCtx<'src> {
    scope: Scope<'src>,
}

struct Scope<'src> {
    stmts: Vec<Stmt<'src>>,
}

pub trait Backend {
    type Codegen;

    fn codegen(ctx: GlobalCtx) -> Self::Codegen;
}

struct LLVM;

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
}
