use {
    crate::{
        abi::TyAbi,
        mir::{self, consts, BinOp, Local, Operand, Rvalue, Statement, StatementKind, Ty},
        IndexVec, Tx,
    },
    abi::{Abi, Size},
    std::ops::Deref,
};

#[derive(Debug)]
pub enum InterpError {
    DivByZero,
    DivOverflow,
    UninitBytes,
    SizeMismatch { target_size: u64, data_size: u64 },
    // Invalid program
    DeadLocal,
    ConstNonsense,
}

pub type InterpResult<T = ()> = Result<T, InterpError>;

#[derive(Debug, Copy, Clone)]
pub enum Scalar {
    Int(mir::ScalarRepr),
}

impl Scalar {
    #[inline]
    pub fn try_to_int(self) -> Result<mir::ScalarRepr, !> {
        match self {
            Scalar::Int(int) => Ok(int),
        }
    }

    #[inline(always)]
    #[cfg_attr(debug_assertions, track_caller)]
    pub fn assert_int(self) -> mir::ScalarRepr {
        self.try_to_int().unwrap()
    }

    #[inline]
    pub fn try_from_uint(data: u128, size: Size) -> Option<Self> {
        mir::ScalarRepr::try_from_uint(data, size).map(Scalar::Int)
    }

    #[inline]
    pub fn to_bits(self, target_size: Size) -> InterpResult<u128> {
        assert_ne!(target_size.bytes(), 0, "you should never look at the bits of a ZST");
        self.try_to_int()
            .map_err(|_| unreachable!("Scalar has one variant"))?
            .to_bits(target_size)
            .map_err(|size| InterpError::SizeMismatch {
                target_size: target_size.bytes(),
                data_size: size.bytes(),
            })
    }

    #[inline]
    pub fn from_uint(data: u128, size: Size) -> Self {
        Self::try_from_uint(data, size).unwrap_or_else(|| {
            panic!("Unsigned value {data:#x} does not fit in {} bits", size.bits())
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Immediate {
    Scalar(Scalar),
    Uninit,
}

impl Immediate {
    #[inline]
    #[cfg_attr(debug_assertions, track_caller)]
    pub fn to_scalar(self) -> Scalar {
        match self {
            Immediate::Scalar(val) => val,
            Immediate::Uninit => panic!("Got uninit where a scalar was expected"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Value<'tcx> {
    imm: Immediate,
    pub layout: TyAbi<'tcx>,
}

impl Value<'_> {
    #[inline]
    pub fn to_const_int(self) -> consts::ConstInt {
        assert!(self.layout.ty.is_integer());
        let int = self.to_scalar().assert_int();
        consts::ConstInt::new(int, self.layout.ty.is_signed(), self.layout.ty.is_ptr_sized())
    }
}

impl Deref for Value<'_> {
    type Target = Immediate;

    fn deref(&self) -> &Self::Target {
        &self.imm
    }
}

impl<'tcx> Value<'tcx> {
    #[inline(always)]
    pub fn from_immediate(imm: Immediate, layout: TyAbi<'tcx>) -> Self {
        debug_assert!(
            match (imm, layout.abi) {
                (Immediate::Scalar(..), Abi::Scalar(..)) => true,
                (Immediate::Uninit, _) if layout.is_sized() => true,
                _ => false,
            },
            "immediate {imm:?} does not fit to layout {layout:?}",
        );
        Self { imm, layout }
    }

    #[inline]
    pub fn from_scalar(scalar: Scalar, layout: TyAbi<'tcx>) -> Self {
        assert!(layout.is_scalar());
        Self { imm: Immediate::Scalar(scalar), layout }
    }

    #[inline]
    pub fn from_bool(tcx: Tx<'tcx>, b: bool) -> Self {
        Self::from_scalar(Scalar::Int(b.into()), tcx.layout_of(tcx.types.bool))
    }

    #[inline]
    pub fn from_uint(data: u128, layout: TyAbi<'tcx>) -> Self {
        Self::from_scalar(Scalar::from_uint(data, layout.size), layout)
    }

    #[inline]
    #[cfg_attr(debug_assertions, track_caller)]
    pub fn to_scalar(self) -> Scalar {
        match self.imm {
            Immediate::Scalar(val) => val,
            Immediate::Uninit => panic!("Got uninit where a scalar was expected"),
        }
    }
}

pub struct Place<'tcx> {
    local: Local,
    pub layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
pub enum LocalValue {
    Dead,
    Live(Immediate),
}

impl LocalValue {
    pub fn make_live_uninit(&mut self) {
        *self = LocalValue::Live(Immediate::Uninit);
    }

    #[inline(always)]
    pub fn access(&self) -> InterpResult<Immediate> {
        match *self {
            LocalValue::Dead => Err(InterpError::DeadLocal),
            LocalValue::Live(val) => Ok(val),
        }
    }

    #[inline(always)]
    pub fn access_mut(&mut self) -> InterpResult<&mut Immediate> {
        match self {
            LocalValue::Dead => Err(InterpError::DeadLocal),
            LocalValue::Live(val) => Ok(val),
        }
    }
}

pub struct Frame<'mir, 'tcx> {
    pub locals: IndexVec<Local, LocalValue>,
    pub body: &'mir mir::Body<'tcx>,
}

pub struct InterpCx<'mir, 'tcx> {
    pub tcx: Tx<'tcx>,
    pub(crate) frame: Frame<'mir, 'tcx>,
}

impl<'mir, 'tcx> InterpCx<'mir, 'tcx> {
    pub fn new(tcx: Tx<'tcx>, body: &'mir mir::Body<'tcx>) -> Self {
        Self {
            tcx,
            frame: Frame { locals: IndexVec::from_elem(LocalValue::Dead, &body.local_decls), body },
        }
    }

    #[inline(always)]
    pub fn sign_extend(&self, value: u128, ty: TyAbi<'tcx>) -> u128 {
        assert!(ty.abi.is_signed());
        ty.size.sign_extend(value)
    }

    #[inline(always)]
    pub fn truncate(&self, value: u128, ty: TyAbi<'tcx>) -> u128 {
        ty.size.truncate(value)
    }

    fn binary_int_op(
        &self,
        bin_op: BinOp,
        a: u128,
        left: TyAbi<'tcx>,
        b: u128,
        right: TyAbi<'tcx>,
    ) -> InterpResult<(Value<'tcx>, bool)> {
        use BinOp::*;

        if let AddUnchecked | SubUnchecked | MulUnchecked = bin_op {
            todo!()
        }

        if left.ty != right.ty {
            panic!("invalid binary op {bin_op:?}: {a:?} ({:?}), {b:?} ({:?})", left.ty, right.ty,)
        }

        let size = left.size;
        let signed = true;

        if signed {
            let op: Option<fn(&i128, &i128) -> bool> = match bin_op {
                Lt => Some(i128::lt),
                Le => Some(i128::le),
                Gt => Some(i128::gt),
                Ge => Some(i128::ge),
                _ => None,
            };
            if let Some(op) = op {
                let a = self.sign_extend(a, left) as i128;
                let b = self.sign_extend(b, right) as i128;
                return Ok((Value::from_bool(self.tcx, op(&a, &b)), false));
            }
            let op: Option<fn(i128, i128) -> (i128, bool)> = match bin_op {
                Div if a == 0 => return Err(InterpError::DivByZero),
                Div => Some(i128::overflowing_div),
                Add | AddUnchecked => Some(i128::overflowing_add),
                Sub | SubUnchecked => Some(i128::overflowing_sub),
                Mul | MulUnchecked => Some(i128::overflowing_mul),
                _ => None,
            };
            if let Some(op) = op {
                let a = self.sign_extend(a, left) as i128;
                let b = self.sign_extend(b, right) as i128;

                if let Div = bin_op
                    && a == size.signed_int_max()
                    && b == -1
                {
                    return Err(InterpError::DivOverflow);
                }

                let (result, overflow) = op(a, b);
                let result = result as u128;

                let truncated = self.truncate(result, left);
                let overflow = overflow || self.sign_extend(truncated, left) != result;

                return Ok((Value::from_uint(truncated, left), overflow));
            }
        }

        Err(InterpError::ConstNonsense)
    }

    #[inline]
    pub fn wrapping_binary_op(
        &self,
        op: BinOp,
        left: Value<'tcx>,
        right: Value<'tcx>,
    ) -> InterpResult<Value<'tcx>> {
        let (val, _overflow) = self.overflowing_binary_op(op, left, right)?;
        Ok(val)
    }

    pub fn overflowing_binary_op(
        &self,
        op: BinOp,
        left: Value<'tcx>,
        right: Value<'tcx>,
    ) -> InterpResult<(Value<'tcx>, bool)> {
        match left.layout.ty.kind() {
            _ if left.layout.ty.is_integer() => {
                assert!(
                    right.layout.ty.is_integer(),
                    "Unexpected types for {op:?}({:?}, {:?})",
                    left.layout.ty,
                    right.layout.ty
                );

                let a = left.to_scalar().to_bits(left.layout.size)?;
                let b = right.to_scalar().to_bits(right.layout.size)?;
                self.binary_int_op(op, a, left.layout, b, right.layout)
            }

            // TODO: use spanned panics
            _ => panic!("Invalid MIR: bad LHS type for binop: {:?}", left.layout.ty),
        }
    }

    pub fn binop_overflow(
        &mut self,
        op: BinOp,
        left: Value<'tcx>,
        right: Value<'tcx>,
        dest: Place<'tcx>,
    ) -> InterpResult<bool> {
        let (val, overflow) = self.overflowing_binary_op(op, left, right)?;
        assert_eq!(val.layout.ty, dest.layout.ty, "type mismatch for result of {op:?}");
        self.write_scalar(val.to_scalar(), dest)?;
        Ok(overflow)
    }

    pub fn binop_ignore_overflow(
        &mut self,
        op: BinOp,
        left: Value<'tcx>,
        right: Value<'tcx>,
        dest: Place<'tcx>,
    ) -> InterpResult {
        let val = self.wrapping_binary_op(op, left, right)?;
        assert_eq!(val.layout.ty, dest.layout.ty, "type mismatch for result of {op:?}");
        self.write_scalar(val.to_scalar(), dest)
    }

    pub fn write_immediate(&mut self, src: Immediate, dest: Place<'tcx>) -> InterpResult {
        assert!(dest.layout.is_sized(), "Cannot write unsized immediate data");

        *self.frame.locals[dest.local].access_mut()? = src;

        Ok(())
    }

    pub fn write_scalar(&mut self, scalar: Scalar, dest: Place<'tcx>) -> InterpResult {
        self.write_immediate(Immediate::Scalar(scalar), dest)
    }

    pub fn read_immediate(&self, src: Value<'tcx>) -> InterpResult<Value<'tcx>> {
        if let Immediate::Uninit = *src {
            return Err(InterpError::UninitBytes);
        }
        Ok(src)
    }

    pub fn read_scalar(&mut self, src: Value<'tcx>) -> InterpResult<Scalar> {
        Ok(self.read_immediate(src)?.to_scalar())
    }

    pub fn eval_const(
        &self,
        (val, ty): (mir::ConstValue, Ty<'tcx>),
        layout: Option<TyAbi<'tcx>>,
    ) -> InterpResult<Value<'tcx>> {
        let imm = match val {
            mir::ConstValue::Scalar(x) => Immediate::Scalar(Scalar::Int(x)),
            mir::ConstValue::Zst => Immediate::Uninit,
        };
        Ok(Value::from_immediate(imm, layout.unwrap_or_else(|| self.tcx.layout_of(ty))))
    }

    fn layout_of_local(&self, local: Local, layout: Option<TyAbi<'tcx>>) -> TyAbi<'tcx> {
        layout.unwrap_or_else(|| self.tcx.layout_of(self.frame.body.local_decls[local].ty))
    }

    pub fn eval_place_copy(
        &self,
        place: mir::Place<'tcx>,
        layout: Option<TyAbi<'tcx>>,
    ) -> InterpResult<Value<'tcx>> {
        if !place.projection.is_empty() {
            return Err(InterpError::ConstNonsense);
        }

        self.eval_local(place.local, layout)
    }

    fn eval_local(&self, local: Local, layout: Option<TyAbi<'tcx>>) -> InterpResult<Value<'tcx>> {
        Ok(Value::from_immediate(
            self.frame.locals[local].access()?,
            self.layout_of_local(local, layout),
        ))
    }

    pub fn eval_place(&self, place: mir::Place<'tcx>) -> InterpResult<Place<'tcx>> {
        if !place.projection.is_empty() {
            return Err(InterpError::ConstNonsense);
        }

        let local = place.local;
        Ok(Place { local, layout: self.layout_of_local(local, None) })
    }

    pub fn eval_operand(
        &self,
        operand: Operand<'tcx>,
        layout: Option<TyAbi<'tcx>>,
    ) -> InterpResult<Value<'tcx>> {
        match operand {
            Operand::Copy(place) => self.eval_place_copy(place, layout),
            Operand::Const(const_, ty) => self.eval_const((const_, ty), layout),
        }
    }

    fn copy_op_no_validate(
        &mut self,
        src: Value<'tcx>,
        dest: Place<'tcx>,
        allow_transmute: bool,
    ) -> InterpResult {
        assert!(!allow_transmute);
        assert!(src.layout == dest.layout);

        let src = self.read_immediate(src)?;
        self.write_immediate(*src, dest)?;

        Ok(())
    }

    pub fn eval_statement(&mut self, statement: &Statement<'tcx>) -> InterpResult {
        match statement.kind {
            StatementKind::Nop => {}
            StatementKind::Assign(place, rvalue) => {
                let dest = self.eval_place(place)?;

                match rvalue {
                    Rvalue::Use(operand) => {
                        let operand = self.eval_operand(operand, Some(dest.layout))?;
                        self.copy_op_no_validate(operand, dest, false)?;
                    }
                    Rvalue::BinaryOp(op, left, right) => {
                        let layout = util::binop_left_homogeneous(op).then_some(dest.layout);
                        let left = self.read_immediate(self.eval_operand(left, layout)?)?;
                        let layout = util::binop_right_homogeneous(op).then_some(left.layout);
                        let right = self.read_immediate(self.eval_operand(right, layout)?)?;

                        // TODO: maybe use lazy binop with `.overflowing_binary_op`
                        if self.binop_overflow(op, left, right, dest)? {
                            return Err(InterpError::ConstNonsense);
                        }
                    }
                    _ => return Err(InterpError::ConstNonsense),
                }
            }
        }

        Ok(())
    }
}

mod util {
    use super::mir::BinOp::{self, *};

    #[inline]
    pub fn binop_left_homogeneous(op: BinOp) -> bool {
        match op {
            Add | AddUnchecked | Sub | SubUnchecked | Mul | MulUnchecked | Div | Offset => true,
            Eq | Ne | Lt | Le | Gt | Ge => false,
        }
    }

    #[inline]
    pub fn binop_right_homogeneous(op: BinOp) -> bool {
        match op {
            Add | AddUnchecked | Sub | SubUnchecked | Mul | MulUnchecked | Div | Eq | Ne | Lt
            | Le | Gt | Ge => true,
            Offset => false,
        }
    }
}
