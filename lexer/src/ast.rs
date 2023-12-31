macro_rules! ast_enum {
    (
        $(#[$enum_attr:meta])*
        pub enum $name:ident $(<$life:lifetime>)? { $($rest:tt)* }
    ) => (
        $(#[$enum_attr])*
        pub enum $name $(<$life>)? { $($rest)* }
    );
}

macro_rules! ast_enum_of_structs {
    (
        $(#[$enum_attr:meta])*
        pub enum $name:ident $(<$life:lifetime>)? { $($body:tt)* }
        $($remaining:tt)*
    ) => {
        ast_enum!($(#[$enum_attr])* pub enum $name $(<$life>)? { $($body)* });
        ast_enum_of_structs_impl!(pub enum $name $(<$life>)? { $($body)* });
    };
}

macro_rules! ast_enum_of_structs_impl {
    (
        pub enum $name:ident <$life:lifetime> {
            $(
                $variant:ident $( ($member:ident $(<$need:lifetime>)?) )*,
            )*
        }
    ) => {
        impl $crate::parse::Spanned for $name<'_> {
            fn span(&self) -> $crate::Span {
                match self {$(
                    $name::$variant(e) => $crate::parse::Spanned::span(e),
                )*}
            }
        }

        $($(
            ast_enum_from_struct!(@lifetime$(($need))? => $name::$variant, $member);
        )*)*
    };

    (
        pub enum $name:ident {
            $(
                $variant:ident $( ($member:ident) )*,
            )*
        }
    ) => {
        impl $crate::parse::Spanned for $name {
            fn span(&self) -> $crate::Span {
                match self {$(
                    $name::$variant(e) => $crate::parse::Spanned::span(e),
                )*}
            }
        }

        $($(
            ast_enum_from_struct!($name::$variant, $member);
        )*)*
    };
}

macro_rules! ast_enum_from_struct {
    ($name:ident::$variant:ident, $member:ident) => {
        impl From<$member> for $name {
            fn from(e: $member) -> $name {
                $name::$variant(e)
            }
        }
    };

    (@lifetime => $name:ident::$variant:ident, $member:ident) => {
        impl<'a> From<$member> for $name<'a> {
            fn from(e: $member) -> Self {
                $name::$variant(e)
            }
        }
    };

    (@lifetime($life:lifetime) => $name:ident::$variant:ident, $member:ident) => {
        impl<$life> From<$member<$life>> for $name<$life> {
            fn from(e: $member<$life>) -> Self {
                $name::$variant(e)
            }
        }
    };
}

use crate::{
    lexer::Lit,
    parse::{Parse, ParseBuffer, Result},
    Lex,
};

impl<'lex> Parse<'lex> for Lit<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> Result<Self> {
        input.step(|step| {
            if let (Lex::Lit(lit), _) = step.next_lex()? {
                Ok(lit)
            } else {
                Err(step.error("expected literal"))
            }
        })
    }
}

#[test]
fn parse_lit() {
    use chumsky::Parser;

    let src = "false \"hello\" 123 0.45";
    let parsed = crate::lexer::lexer().parse(src).into_result().unwrap();
    let mut buf = ParseBuffer::new(parsed);

    for _ in 0..4 {
        let lit: Lit = buf.parse().unwrap();
        println!("{:?}", lit);
    }
}
