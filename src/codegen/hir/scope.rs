use {
    super::{Error, Result, Ty},
    crate::codegen::mir::{self},
    std::collections::HashMap,
};

pub struct Scope<'hir> {
    pub(crate) parent: Option<&'hir mut Self>,
    pub(crate) inner: HashMap<&'hir str, (Ty<'hir>, mir::Place<'hir>)>,
}

impl<'hir> Scope<'hir> {
    pub fn new() -> Self {
        Self { parent: None, inner: HashMap::new() }
    }

    pub fn declare_var(&mut self, ident: &'hir str, ty: (Ty<'hir>, mir::Place<'hir>)) {
        self.inner.insert(ident, ty);
    }

    #[deprecated]
    pub fn use_var(
        &mut self,
        ident: &'hir str,
        (ty, var): (Ty<'hir>, mir::Place<'hir>),
    ) -> Result<'hir, ()> {
        Ok(if let Some((has, _)) = self.inner.get(ident) {
            if ty != *has {
                return Err(Error::TypeMismatch { expected: ty, found: *has });
            }
        } else {
            self.declare_var(ident, (ty, var));
        })
    }

    pub fn get_var(&self, ident: &str) -> Option<(Ty<'hir>, mir::Place<'hir>)> {
        match self.inner.get(ident) {
            Some(found) => Some(*found),
            None => {
                if let Some(parent) = &self.parent {
                    parent.get_var(ident)
                } else {
                    None
                }
            }
        }
    }
}
