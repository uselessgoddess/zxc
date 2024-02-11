use crate::{parse, Ident, Parse, ParseBuffer, Punctuated, Token};

#[derive(Debug, Clone)]
pub struct Path<'lex> {
    pub segments: Punctuated<Ident<'lex>, Token![::]>,
}

impl<'lex> Parse<'lex> for Path<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let mut segments = Punctuated::new();
        loop {
            segments.push_value(input.parse()?);
            if input.peek(Token![::]) {
                segments.push_punct(input.parse()?);
            } else {
                break;
            }
        }
        if segments.is_empty() {
            Err(input.parse::<Ident>().unwrap_err())
        } else if segments.trailing_punct() {
            Err(input.error("expected path segment after `::`"))
        } else {
            Ok(Self { segments })
        }
    }
}

#[test]
fn paths() {
    use crate::util::lex_it;

    let _ = lex_it!("").parse::<Path>().unwrap_err();
    let _ = lex_it!("::").parse::<Path>().unwrap_err();
    let _ = lex_it!("::foo").parse::<Path>().unwrap_err();
    let _ = lex_it!("foo::").parse::<Path>().unwrap_err();

    let path: Path = lex_it!("foo::bar").parse().unwrap();
    assert_eq!(path.segments.len(), 2);
}
