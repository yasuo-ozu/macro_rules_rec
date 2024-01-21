use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::{parse_macro_input, ItemMacro};

mod attribute;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn recursive(attr: TokenStream, input: TokenStream) -> TokenStream {
    attribute::recursive(attr.into(), parse_macro_input!(input as ItemMacro)).into()
}
