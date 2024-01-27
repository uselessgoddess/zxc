use {
    crate::{Level, Lint},
    std::{collections::HashMap, ptr},
};

#[derive(Clone, Copy, Debug)]
pub struct LintId {
    pub lint: &'static Lint,
}

impl LintId {
    pub fn of(lint: &'static Lint) -> LintId {
        LintId { lint }
    }
}

impl PartialEq for LintId {
    fn eq(&self, other: &LintId) -> bool {
        ptr::eq(self.lint, other.lint)
    }
}

impl Eq for LintId {}

impl std::hash::Hash for LintId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.lint as *const Lint).hash(state);
    }
}

pub struct LintStore {
    lints: Vec<&'static Lint>,
    // TODO: extract `FxHashMap` and other structures into new crate
    by_name: HashMap<String, LintId>,
}

impl LintStore {
    pub fn new() -> Self {
        Self { lints: vec![], by_name: Default::default() }
    }

    pub fn lints(&self) -> &[&'static Lint] {
        &self.lints
    }

    pub fn find_lint(&self, lint: &str) -> Option<LintId> {
        self.by_name.get(lint).copied()
    }

    pub fn register_lints(&mut self, lints: &[&'static Lint]) {
        for &lint in lints {
            self.lints.push(lint);

            if self.by_name.insert(lint.name_lower(), LintId::of(lint)).is_some() {
                panic!("duplicate specification of lint {}", lint.name_lower())
            }
        }
    }
}
