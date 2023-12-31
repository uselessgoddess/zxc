pub(crate) mod delim;
pub mod expr;
pub mod item;
mod punct;
mod surround;
pub mod ty;

pub use {
    expr::{BinOp, Block, Expr, Local, Stmt, UnOp},
    item::{Abi, FnArg, ItemFn, ReturnType, Signature},
    punct::Punctuated,
    ty::Type,
};

use {
    crate::{lexer::Token, Lex, Span},
    std::{
        error,
        fmt::{self, Formatter},
        mem::MaybeUninit,
        ops::RangeInclusive,
        ptr::NonNull,
    },
};

pub enum PhantomPeek {}

pub trait Peek: Copy {
    type Token: Token;
}

// TODO(doc): Add notes about this
impl<T: Token, F: FnOnce(PhantomPeek) -> T + Copy> Peek for F {
    type Token = T;
}

macro_rules! define_token {
    { $($pat:pat_param in $ty:ident $(<$($lifetimes:lifetime),+>)? => $display:literal)* } => {$(
        impl$(<$($lifetimes),+>)? Token for $ty$(<$($lifetimes),+>)? {
            fn peek(input: &ParseBuffer) -> bool {
                matches!(input.predict(), Some(($pat, _)))
            }

            fn display() -> &'static str {
                $display
            }
        }

        #[allow(non_snake_case)]
        pub fn $ty $(<$($lifetimes),+>)?(peek: $crate::parse::PhantomPeek)
            -> $ty$(<$($lifetimes),+>)? {
            match peek {}
        }
    )*};
}

pub(crate) use define_token;

#[derive(Debug)]
pub enum ErrorKind {
    Eof,
    Expect(&'static str),
    Expected { expected: Vec<&'static str>, found: &'static str },
    Custom(String),
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

    pub fn eof(span: Span) -> Self {
        Self::new(span, "end of input")
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}

impl error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse<'lex>: Sized + 'lex {
    fn parse(input: &mut ParseBuffer<'lex>) -> Result<Self>;
}

impl<'lex, T: Parse<'lex>> Parse<'lex> for Box<T> {
    fn parse(input: &mut ParseBuffer<'lex>) -> Result<Self> {
        input.parse().map(Box::new)
    }
}

impl<'lex, T: Parse<'lex> + Token> Parse<'lex> for Option<T> {
    fn parse(input: &mut ParseBuffer<'lex>) -> Result<Self> {
        Ok(if T::peek(input) { Some(input.parse()?) } else { None })
    }
}

type Walker<'lex> = unsafe fn(&MaybeUninit<(Lex<'lex>, Span)>) -> (Lex<'lex>, Span);

unsafe fn clone_walker<'lex>(lex: &MaybeUninit<(Lex<'lex>, Span)>) -> (Lex<'lex>, Span) {
    MaybeUninit::assume_init_ref(lex).clone()
}

pub struct ParseBuffer<'lex> {
    owner: bool,
    tokens: NonNull<[MaybeUninit<(Lex<'lex>, Span)>]>,
    walker: Walker<'lex>,
    cursor: usize,
    span: Span,
}

pub struct Advance<'stream, 'lex>(&'stream mut ParseBuffer<'lex>);

impl Advance<'_, '_> {
    fn to(&mut self, fork: &mut ParseBuffer) {
        fn in_scope<T>(scope: NonNull<[T]>, ptr: NonNull<[T]>) -> bool {
            fn as_range<T>(slice: *const [T]) -> RangeInclusive<*const T> {
                let start = slice as *const T;
                start..=unsafe { start.add(slice.len()) }
            }

            let scope = as_range(scope.as_ptr());
            let ptr = as_range(ptr.as_ptr());

            scope.contains(ptr.start()) && scope.contains(ptr.end())
        }

        if !in_scope(self.0.tokens, fork.tokens) {
            panic!("Fork was not derived from the advancing parse stream");
        }

        self.0.cursor = fork.cursor;
    }
}

impl<'lex> ParseBuffer<'lex> {
    // TODO: accept `Box<[..]>`
    pub fn new(tokens: Vec<(Lex<'lex>, Span)>) -> Self {
        // Safety: safe because works as coercion `Lex` -> `MaybeUninit<Lex>`
        let slice = Box::leak(tokens.into_boxed_slice());

        let walker = MaybeUninit::assume_init_read;
        let tokens = unsafe {
            NonNull::slice_from_raw_parts(
                NonNull::new_unchecked(slice.as_mut_ptr().cast()),
                slice.len(),
            )
        };
        Self { owner: true, tokens, walker, cursor: 0, span: Span::splat(0) }
    }

    #[deprecated(note = "possible but unlikely")]
    #[allow(dead_code)]
    pub(crate) fn listen(tokens: &mut [(Lex<'lex>, Span)]) -> Self {
        let tokens = unsafe {
            NonNull::slice_from_raw_parts(
                NonNull::new_unchecked(tokens.as_mut_ptr().cast()),
                tokens.len(),
            )
        };
        Self { owner: false, tokens, walker: clone_walker, cursor: 0, span: Span::splat(0) }
    }

    unsafe fn make_clone<'a: 'b, 'b>(&ParseBuffer { tokens, cursor, span, .. }: &Self) -> Self {
        ParseBuffer { owner: false, walker: clone_walker, tokens, cursor, span }
    }

    pub fn step<F, P>(&mut self, parse: F) -> Result<P>
    where
        F: FnOnce(&mut ParseBuffer<'lex>) -> Result<P>,
    {
        let mut rest = unsafe { Self::make_clone(self) };

        let parsed = parse(&mut rest)?;
        self.cursor = rest.cursor;
        Ok(parsed)
    }

    pub fn fork<F, R>(&mut self, parse: F) -> R
    where
        F: FnOnce(&mut ParseBuffer<'lex>, Advance<'_, '_>) -> R,
    {
        parse(&mut unsafe { Self::make_clone(self) }, Advance(self))
    }

    // TODO: maybe store `cursor` in the `Cell`
    pub fn scan<F, R>(&self, parse: F) -> R
    where
        F: FnOnce(&mut ParseBuffer<'lex>) -> R,
    {
        parse(&mut unsafe { Self::make_clone(self) })
    }

    pub fn error(&self, message: impl fmt::Display) -> Error {
        Error::new(self.span(), message.to_string()) // TODO: add checks for EOF
    }

    pub fn is_empty(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    pub fn predict(&self) -> Option<&(Lex<'lex>, Span)> {
        // Safety: we unitialize after `next_lex`

        if self.is_empty() {
            None
        } else {
            unsafe {
                Some(MaybeUninit::assume_init_ref(
                    self.tokens.get_unchecked_mut(self.cursor).as_ref(),
                ))
            }
        }
    }

    pub fn do_in<T>(&mut self, f: fn(&mut ParseBuffer<'lex>) -> Result<T>) -> Result<T> {
        f(self)
    }

    pub fn span(&self) -> Span {
        if let Some((_, span)) = self.predict() { *span } else { self.span }
    }

    pub fn peek<P: Peek>(&self, peek: P) -> bool {
        let _ = peek;
        self.peek_raw::<P::Token>()
    }

    pub fn peek_raw<T: Token>(&self) -> bool {
        T::peek(self)
    }

    pub fn parse<P: Parse<'lex>>(&mut self) -> Result<P> {
        P::parse(self)
    }

    pub fn parse_terminated<T, P>(
        &mut self,
        parser: fn(&mut ParseBuffer<'lex>) -> Result<T>,
        separator: P,
    ) -> Result<Punctuated<T, P::Token>>
    where
        P: Peek,
        P::Token: Parse<'lex>,
    {
        let _ = separator;
        Punctuated::parse_terminated_with(self, parser)
    }

    pub fn custom<T>(&mut self, parser: fn(&mut ParseBuffer<'lex>) -> Result<T>) -> Result<T> {
        parser(self)
    }

    pub fn lookahead<'ahead>(&'ahead mut self) -> Lookahead<'ahead, 'lex> {
        Lookahead { parent: self, peeks: Vec::new() }
    }

    fn next_lex_impl(&mut self, walker: Walker<'lex>) -> Result<(Lex<'lex>, Span)> {
        if self.is_empty() {
            Err(Error::eof(self.span))
        } else {
            self.cursor += 1;
            // Safety: unitialize after step next
            Ok(unsafe { walker(self.tokens.get_unchecked_mut(self.cursor - 1).as_mut()) })
                .inspect(|(_, span)| self.span = *span)
        }
    }

    pub fn skip_next(&mut self) -> Result<Span> {
        unsafe {
            if self.is_empty() {
                Err(Error::eof(self.span))
            } else {
                let (_, span) = MaybeUninit::assume_init_ref(
                    self.tokens.get_unchecked_mut(self.cursor).as_ref(),
                );
                self.span = *span;
                self.cursor += 1;
                Ok(self.span)
            }
        }
    }

    pub(crate) fn next_lex(&mut self) -> Result<(Lex<'lex>, Span)> {
        self.next_lex_impl(self.walker)
    }

    #[allow(dead_code)]
    pub(crate) fn next_lex_clone(&mut self) -> Result<(Lex<'lex>, Span)> {
        self.next_lex_impl(clone_walker)
    }

    pub(crate) fn parse_delimited<Lt: Token + Parse<'lex>, Rt: Token + Parse<'lex>>(
        &mut self,
    ) -> Result<((Lt, Rt), ParseBuffer<'lex>)> {
        let anchor = self.cursor;

        let lt: Lt = self.parse()?;

        let mut balance: isize = 1;
        loop {
            if self.peek_raw::<Rt>() {
                balance -= 1;
            } else if self.peek_raw::<Lt>() {
                balance += 1;
            }

            if balance == 0 {
                break;
            }
            self.skip_next()?;
        }

        let rt: Rt = self.parse()?;

        let tokens = NonNull::slice_from_raw_parts(self.tokens.as_non_null_ptr(), self.cursor - 1);
        Ok((
            (lt, rt),
            ParseBuffer {
                owner: false,
                tokens,
                walker: self.walker,
                cursor: anchor + 1,
                span: self.span,
            },
        ))
    }
}

impl Drop for ParseBuffer<'_> {
    fn drop(&mut self) {
        if !self.owner {
            return;
        }

        unsafe {
            let mut slice = Box::from_raw(self.tokens.as_ptr());
            for lex in &mut slice[self.cursor..] {
                MaybeUninit::assume_init_drop(lex)
            }
        }
    }
}

pub struct Lookahead<'parent, 'lex> {
    parent: &'parent mut ParseBuffer<'lex>,
    peeks: Vec<&'static str>,
}

impl<'parent, 'lex> Lookahead<'parent, 'lex> {
    pub fn peek<P: Peek>(&mut self, peek: P) -> bool {
        if self.parent.peek(peek) {
            true
        } else {
            self.peeks.push(P::Token::display());
            false
        }
    }

    pub fn error(self) -> Error {
        let input = self.parent;
        match self.peeks[..] {
            [] => {
                if input.is_empty() {
                    Error::eof(input.span)
                } else {
                    input.error("unexpected token")
                }
            }
            [expected] => input.error(format!("expected {expected}")),
            [a, b] => input.error(format!("expected {a} or {b}")),
            ref peeks => input.error(format!("expected one of: {}", peeks.join(", "))),
        }
    }
}

pub use {
    delim::{lookahead, DelimSpan},
    surround::Spanned,
};
