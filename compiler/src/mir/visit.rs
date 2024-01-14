use {
    crate::{
        mir::{
            BasicBlock, BasicBlockData, Body, ConstValue, Local, LocalDecl, Location, Operand,
            Place, PlaceElem, PlaceRef, Rvalue, Statement, Terminator, Ty,
        },
        Tx,
    },
    std::borrow::Cow,
};

macro_rules! make_mir_visitor {
    ($visitor_trait_name:ident, $($mutability:ident)?) => {
        pub trait $visitor_trait_name<'tcx> {

            fn visit_body(
                &mut self,
                body: &$($mutability)? Body<'tcx>,
            ) {
                self.super_body(body);
            }

            extra_body_methods!($($mutability)?);

            fn visit_basic_block_data(
                &mut self,
                block: BasicBlock,
                data: & $($mutability)? BasicBlockData<'tcx>,
            ) {
                self.super_basic_block_data(block, data);
            }

            fn visit_statement(
                &mut self,
                statement: & $($mutability)? Statement<'tcx>,
                location: Location,
            ) {
                self.super_statement(statement, location);
            }

            fn visit_assign(
                &mut self,
                place: & $($mutability)? Place<'tcx>,
                rvalue: & $($mutability)? Rvalue<'tcx>,
                location: Location,
            ) {
                self.super_assign(place, rvalue, location);
            }

            fn visit_terminator(
                &mut self,
                terminator: & $($mutability)? Terminator<'tcx>,
                location: Location,
            ) {
                self.super_terminator(terminator, location);
            }

            fn visit_rvalue(
                &mut self,
                rvalue: & $($mutability)? Rvalue<'tcx>,
                location: Location,
            ) {
                self.super_rvalue(rvalue, location);
            }

            fn visit_operand(
                &mut self,
                operand: & $($mutability)? Operand<'tcx>,
                location: Location,
            ) {
                self.super_operand(operand, location);
            }

            fn visit_place(
                &mut self,
                place: & $($mutability)? Place<'tcx>,
                context: PlaceContext,
                location: Location,
            ) {
                self.super_place(place, context, location);
            }

            visit_place_fns!($($mutability)?);

            /// This is called for every constant in the MIR body and every `required_consts`
            /// (i.e., including consts that have been dead-code-eliminated).
            fn visit_constant(
                &mut self,
                constant: (& $($mutability)? ConstValue, & $($mutability)? Ty<'tcx>),
                location: Location,
            ) {
                self.super_constant(constant, location);
            }

            fn visit_ty(
                &mut self,
                ty: $(& $mutability)? Ty<'tcx>,
                _: TyContext,
            ) {
                self.super_ty(ty);
            }

            fn visit_local_decl(
                &mut self,
                local: Local,
                local_decl: & $($mutability)? LocalDecl<'tcx>,
            ) {
                self.super_local_decl(local, local_decl);
            }

            fn visit_local(
                &mut self,
                _local: $(& $mutability)? Local,
                _context: PlaceContext,
                _location: Location,
            ) {}

            // The `super_xxx` methods comprise the default behavior and are
            // not meant to be overridden.

            fn super_body(
                &mut self,
                body: &$($mutability)? Body<'tcx>,
            ) {
                super_body!(self, body, $($mutability, true)?);
            }

            fn super_basic_block_data(&mut self,
                                      block: BasicBlock,
                                      data: & $($mutability)? BasicBlockData<'tcx>) {
                let BasicBlockData {
                    statements,
                    terminator,
                } = data;

                let mut index = 0;
                for statement in statements {
                    let location = Location { block, statement_index: index };
                    self.visit_statement(statement, location);
                    index += 1;
                }

                if let Some(terminator) = terminator {
                    let location = Location { block, statement_index: index };
                    self.visit_terminator(terminator, location);
                }
            }

            fn super_statement(&mut self,
                               statement: & $($mutability)? Statement<'tcx>,
                               location: Location) {
                match statement {
                    Statement::Assign(
                        place, rvalue
                    ) => {
                        self.visit_assign(place, rvalue, location);
                    }
                    Statement::Nop => {}
                }
            }

            fn super_assign(&mut self,
                            place: &$($mutability)? Place<'tcx>,
                            rvalue: &$($mutability)? Rvalue<'tcx>,
                            location: Location) {
                self.visit_place(
                    place,
                    PlaceContext::MutatingUse(MutatingUseContext::Store),
                    location
                );
                self.visit_rvalue(rvalue, location);
            }

            fn super_terminator(&mut self,
                                terminator: &$($mutability)? Terminator<'tcx>,
                                location: Location) {
                match terminator {
                    Terminator::Goto { .. } | Terminator::Unreachable => {}
                    Terminator::Return => {
                        // `return` logically moves from the return place `_0`. Note that the place
                        // cannot be changed by any visitor, though.
                        let $($mutability)? local = Local::RETURN_PLACE;
                        self.visit_local(
                            $(& $mutability)? local,
                            PlaceContext::NonMutatingUse(NonMutatingUseContext::Move),
                            location,
                        );

                        assert_eq!(
                            local,
                            Local::RETURN_PLACE,
                            "`MutVisitor` tried to mutate return place of `return` terminator"
                        );
                    }

                    Terminator::SwitchInt {
                        discr,
                        targets: _
                    } => {
                        self.visit_operand(discr, location);
                    }

                    Terminator::Call {
                        func,
                        args,
                        dest,
                        target: _,
                        fn_span: _
                    } => {
                        self.visit_operand(func, location);
                        for arg in args {
                            self.visit_operand(arg, location);
                        }
                        self.visit_place(
                            dest,
                            PlaceContext::MutatingUse(MutatingUseContext::Call),
                            location
                        );
                    }
                }
            }

            fn super_rvalue(&mut self,
                            rvalue: & $($mutability)? Rvalue<'tcx>,
                            location: Location) {
                match rvalue {
                    Rvalue::Use(operand) => {
                        self.visit_operand(operand, location);
                    }

                    Rvalue::UseDeref(place) => {
                        self.visit_place(
                            place,
                            PlaceContext::NonMutatingUse(NonMutatingUseContext::Inspect),
                            location
                        );
                    }

                    Rvalue::Cast(_cast_kind, operand, ty) => {
                        self.visit_operand(operand, location);
                        self.visit_ty($(& $mutability)? *ty, TyContext::Location(location));
                    }

                    Rvalue::BinaryOp(_bin_op, lhs, rhs) => {
                        self.visit_operand(lhs, location);
                        self.visit_operand(rhs, location);
                    }

                    Rvalue::UnaryOp(_un_op, op) => {
                        self.visit_operand(op, location);
                    }
                }
            }

            fn super_operand(&mut self,
                             operand: & $($mutability)? Operand<'tcx>,
                             location: Location) {
                match operand {
                    Operand::Copy(place) => {
                        self.visit_place(
                            place,
                            PlaceContext::NonMutatingUse(NonMutatingUseContext::Copy),
                            location
                        );
                    }
                    Operand::Const(const_, ty) => {
                        self.visit_constant((const_, ty), location);
                    }
                }
            }

            fn super_local_decl(&mut self,
                                local: Local,
                                local_decl: & $($mutability)? LocalDecl<'tcx>) {
                let LocalDecl {
                    mutability: _,
                    ty,
                } = local_decl;

                self.visit_ty($(& $mutability)? *ty, TyContext::LocalDecl {
                    local,
                });
            }

            fn super_constant(
                &mut self,
                (_, ty): (& $($mutability)? ConstValue, & $($mutability)? Ty<'tcx>),
                location: Location
            ) {
                self.visit_ty($(& $mutability)? *ty, TyContext::Location(location))
            }

            fn super_ty(&mut self, _ty: $(& $mutability)? Ty<'tcx>) {
            }

            // Convenience methods

            fn visit_location(
                &mut self,
                body: &$($mutability)? Body<'tcx>,
                location: Location
            ) {
                let basic_block = & $($mutability)?
                    basic_blocks!(body, $($mutability, true)?)[location.block];
                if basic_block.statements.len() == location.statement_index {
                    if let Some(ref $($mutability)? terminator) = basic_block.terminator {
                        self.visit_terminator(terminator, location)
                    }
                } else {
                    let statement = & $($mutability)?
                        basic_block.statements[location.statement_index];
                    self.visit_statement(statement, location)
                }
            }
        }
    }
}

macro_rules! basic_blocks {
    ($body:ident, mut, true) => {
        (&mut $body.basic_blocks)
    };
    ($body:ident, mut, false) => {
        // TODO: .as_mut_preserves_cfg()
        (&mut $body.basic_blocks)
    };
    ($body:ident,) => {
        $body.basic_blocks
    };
}

macro_rules! basic_blocks_iter {
    ($body:ident, mut, $invalidate:tt) => {
        basic_blocks!($body, mut, $invalidate).iter_enumerated_mut()
    };
    ($body:ident,) => {
        basic_blocks!($body,).iter_enumerated()
    };
}

macro_rules! extra_body_methods {
    (mut) => {
        fn visit_body_preserves_cfg(&mut self, body: &mut Body<'tcx>) {
            self.super_body_preserves_cfg(body);
        }

        fn super_body_preserves_cfg(&mut self, body: &mut Body<'tcx>) {
            super_body!(self, body, mut, false);
        }
    };
    () => {};
}

macro_rules! super_body {
    ($self:ident, $body:ident, $($mutability:ident, $invalidate:tt)?) => {
        for (bb, data) in basic_blocks_iter!($body, $($mutability, $invalidate)?) {
            $self.visit_basic_block_data(bb, data);
        }

        $self.visit_ty(
            $(& $mutability)? $body.return_ty(),
            TyContext::ReturnTy
        );

        for local in $body.local_decls.indices() {
            $self.visit_local_decl(local, & $($mutability)? $body.local_decls[local]);
        }
    }
}

macro_rules! visit_place_fns {
    (mut) => {
        fn tcx<'a>(&'a self) -> Tx<'tcx>;

        fn super_place(
            &mut self,
            place: &mut Place<'tcx>,
            context: PlaceContext,
            location: Location,
        ) {
            self.visit_local(&mut place.local, context, location);

            if let Some(new_projection) = self.process_projection(&place.projection, location) {
                place.projection = self.tcx().mk_place_elems(&new_projection);
            }
        }

        fn process_projection<'a>(
            &mut self,
            projection: &'a [PlaceElem<'tcx>],
            location: Location,
        ) -> Option<Vec<PlaceElem<'tcx>>> {
            let mut projection = Cow::Borrowed(projection);

            for i in 0..projection.len() {
                if let Some(&elem) = projection.get(i) {
                    if let Some(elem) = self.process_projection_elem(elem, location) {
                        // This converts the borrowed projection into `Cow::Owned(_)` and returns a
                        // clone of the projection so we can mutate and reintern later.
                        let vec = projection.to_mut();
                        vec[i] = elem;
                    }
                }
            }

            match projection {
                Cow::Borrowed(_) => None,
                Cow::Owned(vec) => Some(vec),
            }
        }

        fn process_projection_elem(
            &mut self,
            elem: PlaceElem<'tcx>,
            location: Location,
        ) -> Option<PlaceElem<'tcx>> {
            match elem {
                PlaceElem::Subtype(ty) => {
                    let mut new_ty = ty;
                    self.visit_ty(&mut new_ty, TyContext::Location(location));
                    if ty != new_ty { Some(PlaceElem::Subtype(new_ty)) } else { None }
                }
                PlaceElem::Deref => None,
            }
        }
    };

    () => {
        fn visit_projection(
            &mut self,
            place_ref: PlaceRef<'tcx>,
            context: PlaceContext,
            location: Location,
        ) {
            self.super_projection(place_ref, context, location);
        }

        fn visit_projection_elem(
            &mut self,
            place_ref: PlaceRef<'tcx>,
            elem: PlaceElem<'tcx>,
            context: PlaceContext,
            location: Location,
        ) {
            self.super_projection_elem(place_ref, elem, context, location);
        }

        fn super_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
            let mut context = context;

            if !place.projection.is_empty() {
                if context.is_use() {
                    // ^ Only change the context if it is a real use, not a "use" in debuginfo.
                    context = if context.is_mutating_use() {
                        PlaceContext::MutatingUse(MutatingUseContext::Projection)
                    } else {
                        PlaceContext::NonMutatingUse(NonMutatingUseContext::Projection)
                    };
                }
            }

            self.visit_local(place.local, context, location);

            self.visit_projection(place.as_ref(), context, location);
        }

        fn super_projection(
            &mut self,
            place_ref: PlaceRef<'tcx>,
            context: PlaceContext,
            location: Location,
        ) {
            for (base, elem) in place_ref.iter_projections().rev() {
                self.visit_projection_elem(base, elem, context, location);
            }
        }

        fn super_projection_elem(
            &mut self,
            _place_ref: PlaceRef<'tcx>,
            elem: PlaceElem<'tcx>,
            _context: PlaceContext,
            location: Location,
        ) {
            match elem {
                PlaceElem::Subtype(ty) => {
                    self.visit_ty(ty, TyContext::Location(location));
                }
                PlaceElem::Deref => {}
            }
        }
    };
}

make_mir_visitor!(Visitor,);
make_mir_visitor!(MutVisitor, mut);

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum TyContext {
    LocalDecl {
        /// The index of the local variable we are visiting.
        local: Local,
    },
    /// The return type of the function.
    ReturnTy,
    /// A type found at some location.
    Location(Location),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NonMutatingUseContext {
    /// Being inspected in some way, like loading a len.
    Inspect,
    /// Consumed as part of an operand.
    Copy,
    /// Consumed as part of an operand.
    Move,
    /// Shared borrow.
    SharedBorrow,
    /// A fake borrow.
    FakeBorrow,
    /// AddressOf for *const pointer.
    AddressOf,
    /// PlaceMention statement.
    ///
    /// This statement is executed as a check that the `Place` is live without reading from it,
    /// so it must be considered as a non-mutating use.
    PlaceMention,
    /// Used as base for another place, e.g., `x` in `x.y`. Will not mutate the place.
    /// For example, the projection `x.y` is not marked as a mutation in these cases:
    /// ```ignore (illustrative)
    /// z = x.y;
    /// f(&x.y);
    /// ```
    Projection,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MutatingUseContext {
    /// Appears as LHS of an assignment.
    Store,
    /// Appears on `SetDiscriminant`
    SetDiscriminant,
    /// Appears on `Deinit`
    Deinit,
    /// Output operand of an inline assembly block.
    AsmOutput,
    /// Destination of a call.
    Call,
    /// Destination of a yield.
    Yield,
    /// Being dropped.
    Drop,
    /// Mutable borrow.
    Borrow,
    /// AddressOf for *mut pointer.
    AddressOf,
    /// Used as base for another place, e.g., `x` in `x.y`. Could potentially mutate the place.
    /// For example, the projection `x.y` is marked as a mutation in these cases:
    /// ```ignore (illustrative)
    /// x.y = ...;
    /// f(&mut x.y);
    /// ```
    Projection,
    /// Retagging, a "Stacked Borrows" shadow state operation
    Retag,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PlaceContext {
    NonMutatingUse(NonMutatingUseContext),
    MutatingUse(MutatingUseContext),
}

impl PlaceContext {
    pub fn is_use(&self) -> bool {
        true
    }

    pub fn is_mutating_use(&self) -> bool {
        matches!(self, Self::MutatingUse(_))
    }
}
