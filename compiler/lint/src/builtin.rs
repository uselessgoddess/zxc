use crate::{declare_lint, declare_lints_pass};

declare_lint! {
    pub arithmetic_overflow,
    Deny,
    "arithmetic operation overflows"
}

declare_lint! {
    pub overflowing_literals,
    Deny,
    "literal out of range for its type"
}

declare_lints_pass! {
    HardwiredLints => [
        arithmetic_overflow,
        overflowing_literals,
    ]
}
