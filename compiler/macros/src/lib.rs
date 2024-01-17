#![feature(let_chains)]

mod data_layout;
mod symbols;

#[proc_macro]
pub fn symbols(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    symbols::symbols(input)
}

#[proc_macro]
pub fn target_data_layout(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    data_layout::imp(input.into()).into()
}
