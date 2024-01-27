use crate::{symbol::Symbol, FxHashMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LevelSource {
    Default,
    CommandLine(Symbol, Level),
}

pub(super) fn lint_levels_on_raw(
    store: &LintStore,
    lints_opts: &[(String, Level)],
) -> FxHashMap<LintId, (Level, LevelSource)> {
    let mut lints: FxHashMap<_, _> = store
        .lints()
        .iter()
        .map(|&lint| (LintId::of(lint), (lint.default_level, LevelSource::Default)))
        .collect();

    for &(ref name, level) in lints_opts {
        if let Some(id) = store.find_lint(name) {
            lints.insert(id, (level, LevelSource::CommandLine(Symbol::intern(name), level)));
        } else {
            // TODO: emit warn?
        }
    }
    lints
}

pub use lint::*;
