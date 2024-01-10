use anstyle::{AnsiColor, AnsiColor::*, Effects, Style};

pub const fn style(color: AnsiColor) -> Style {
    color.on_default().effects(Effects::BOLD)
}

pub const HEADER: Style = style(Green);
pub const USAGE: Style = style(Green);
pub const LITERAL: Style = style(Cyan);
pub const PLACEHOLDER: Style = Cyan.on_default();
pub const ERROR: Style = style(Red);
pub const VALID: Style = style(Cyan);
pub const INVALID: Style = style(Yellow);

use clap::builder::Styles;

pub const CLAP: Styles = Styles::styled()
    .header(HEADER)
    .usage(USAGE)
    .literal(LITERAL)
    .placeholder(PLACEHOLDER)
    .error(ERROR)
    .valid(VALID)
    .invalid(INVALID);
