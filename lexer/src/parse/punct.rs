use {
    super::Result,
    crate::{
        parse::{surround::lookahead_span, Parse, ParseBuffer, Spanned},
        Span,
    },
    std::{fmt, ops::Index},
};

pub enum Pair<T, P> {
    Punctuated(T, P),
    End(T),
}

#[derive(Clone)]
pub struct Punctuated<T, P> {
    inner: Vec<(T, P)>,
    last: Option<Box<T>>,
}

impl<T, P> Punctuated<T, P> {
    pub const fn new() -> Self {
        Self { inner: Vec::new(), last: None }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0 && self.last.is_none()
    }

    pub fn len(&self) -> usize {
        self.inner.len() + if self.last.is_some() { 1 } else { 0 }
    }

    pub fn first(&self) -> Option<&T> {
        self.iter().next()
    }

    pub fn first_mut(&mut self) -> Option<&mut T> {
        self.iter_mut().next()
    }

    pub fn last(&self) -> Option<&T> {
        self.iter().next_back()
    }

    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.iter_mut().next_back()
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.inner.iter().map(|(item, _)| item).chain(self.last.as_deref())
    }

    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut T> {
        self.inner.iter_mut().map(|(item, _)| item).chain(self.last.as_deref_mut())
    }

    pub fn pairs(&self) -> impl Iterator<Item = Pair<&T, &P>> {
        self.inner
            .iter()
            .map(|(t, p)| Pair::Punctuated(t, p))
            .chain(self.last.as_deref().map(Pair::End))
    }

    pub fn push_value(&mut self, value: T) {
        assert!(self.empty_or_trailing());

        self.last = Some(Box::new(value));
    }

    pub fn push_punct(&mut self, punctuation: P) {
        assert!(self.last.is_some());

        let last = self.last.take().unwrap();
        self.inner.push((*last, punctuation));
    }

    pub fn pop(&mut self) -> Option<Pair<T, P>> {
        if self.last.is_some() {
            self.last.take().map(|t| Pair::End(*t))
        } else {
            self.inner.pop().map(|(t, p)| Pair::Punctuated(t, p))
        }
    }

    pub fn pop_punct(&mut self) -> Option<P> {
        if self.last.is_some() {
            None
        } else {
            let (t, p) = self.inner.pop()?;
            self.last = Some(Box::new(t));
            Some(p)
        }
    }

    pub fn trailing_punct(&self) -> bool {
        self.last.is_none() && !self.is_empty()
    }

    pub fn empty_or_trailing(&self) -> bool {
        self.last.is_none()
    }

    pub fn push(&mut self, value: T)
    where
        P: Default,
    {
        if !self.empty_or_trailing() {
            self.push_punct(Default::default());
        }
        self.push_value(value);
    }

    pub fn parse_terminated_with<'lex>(
        input: &mut ParseBuffer<'lex>,
        parser: fn(&mut ParseBuffer<'lex>) -> Result<T>,
    ) -> Result<Self>
    where
        P: Parse<'lex>,
    {
        let mut punctuated = Punctuated::new();

        loop {
            if input.is_empty() {
                break;
            }
            let value = parser(input)?;
            punctuated.push_value(value);
            if input.is_empty() {
                break;
            }
            let punct = input.parse()?;
            punctuated.push_punct(punct);
        }

        Ok(punctuated)
    }

    #[deprecated]
    #[allow(dead_code)]
    pub(crate) fn predict_span(punct: &Punctuated<T, P>) -> Option<Span>
    where
        T: Spanned,
        P: Spanned,
    {
        Some(match (punct.inner.first(), &punct.last) {
            (Some((first, _)), Some(last)) => lookahead_span(first.span(), last.span()),
            (Some((first, _)), None) => {
                lookahead_span(first.span(), punct.inner.last().unwrap().1.span())
            }
            (None, Some(last)) => last.span(),
            _ => return None,
        })
    }
}

impl<T, P> Index<usize> for Punctuated<T, P> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        if index == self.len() - 1 {
            match &self.last {
                Some(t) => t,
                None => &self.inner[index].0,
            }
        } else {
            &self.inner[index].0
        }
    }
}

impl<T: fmt::Debug, P: fmt::Debug> fmt::Debug for Punctuated<T, P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut list = f.debug_list();
        for (t, p) in &self.inner {
            list.entry(t);
            list.entry(p);
        }
        if let Some(last) = &self.last {
            list.entry(last);
        }
        list.finish()
    }
}

#[test]
fn parse_terminated() {
    use crate::{lexer::Lit, util::lex_it, Token};

    {
        let mut input = lex_it!("10, 11, 12");
        let punct = input.parse_terminated(Lit::parse, Token![,]).unwrap();
        assert!(!punct.trailing_punct());
    }

    {
        let mut input = lex_it!("10, 11,");
        let punct = input.parse_terminated(Lit::parse, Token![,]).unwrap();
        assert!(punct.trailing_punct());
    }
}
