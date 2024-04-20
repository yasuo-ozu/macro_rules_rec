The `macro_rules_rec` crate enables recursive calls to itself within `macro_rules!` in the Rust language.

In Rust, macros, especially defined with `macro_rules!` expands the macro's body in-place when used. This means that all intermediate states produced during the expansion must adhere to Rust's syntax.

Namely, when a macro recursively calls itself within its body, the position where this call is written must be within a syntactic element that supports macro invocation according to Rust's syntax. Typically, many macros are intended to expand into expressions, statements, types, or declaration items, and this rarely poses a problem. However, in special cases like the following, the position of the macro call does not correspond to any syntactic element in Rust, which traditionally prevented the recursive invocation of the macro:

- When generating pairs of types and constraints simultaneously in the where clause of generics using a single macro.
- When generating pairs of field names and types simultaneously within a struct declaration using a single macro.

`macro_rules_rec` extends the declarative macro writing with Rust's `macro_rules!` and enables recursive macro calls that are not constrained by Rust's syntax. Additionally, the macros generated by `macro_rules_rec` are pure `macro_rules!` and do not depend on external crates (including `macro_rules_rec`). This is very useful when creating libraries, as users of the library do not need to reference `macro_rules_rec` when using macros generated with it.

## Example

You can perform self-recursive call via `$self!` sentence.

```rust
# use macro_rules_rec::recursive;
#[recursive]
macro_rules! m {
    ($($id:ident)*) => {
        $(
            $self!(@ $id);
        )*
    };
    (@ $id:ident) => {
        #[allow(unused)]
        let $id = 123;
    };
}
m!(a b c);
```