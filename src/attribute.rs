use proc_macro2::{Delimiter, Group, Spacing, Span, TokenStream, TokenTree};
use proc_macro_error::abort;
use std::collections::VecDeque;
use syn::spanned::Spanned;
use syn::*;
use template_quote::quote;

struct MacroRulesBody(Vec<(Group, Token![=>], Group, Token![;])>);

impl parse::Parse for MacroRulesBody {
    fn parse(input: parse::ParseStream<'_>) -> Result<Self> {
        let mut v = Vec::new();
        while !input.is_empty() {
            let g: Group = input.parse()?;
            if g.delimiter() != Delimiter::Parenthesis {
                abort!(g.span(), "Should be parenthesis");
            }
            let ar = input.parse()?;
            let b: Group = input.parse()?;
            if b.delimiter() != Delimiter::Brace {
                abort!(b.span(), "Should be brace");
            }
            v.push((g, ar, b, input.parse()?));
        }
        Ok(MacroRulesBody(v))
    }
}

fn getrandom() -> u64 {
    use core::hash::{BuildHasher, Hash, Hasher};
    use std::collections::hash_map::RandomState;

    let mut hasher = RandomState::new().build_hasher();
    format!("{:?}", &Span::call_site()).hash(&mut hasher);
    hasher.finish()
}

fn remove_macro_arg_kind(input: TokenStream) -> TokenStream {
    let mut input: VecDeque<_> = input.into_iter().collect();
    let mut ret = TokenStream::new();
    loop {
        match input.pop_front() {
            Some(TokenTree::Punct(p1)) if p1.as_char() == '$' && p1.spacing() == Spacing::Alone => {
                match input.pop_front() {
                    Some(TokenTree::Ident(id)) => {
                        match input.pop_front() {
                            Some(TokenTree::Punct(p2))
                                if p2.as_char() == ':' && p2.spacing() == Spacing::Alone =>
                            {
                                match input.pop_front() {
                                    Some(TokenTree::Ident(_)) => {
                                        ret.extend(Some(TokenTree::Punct(p1)));
                                        ret.extend(Some(TokenTree::Ident(id)));
                                        continue;
                                    }
                                    Some(o) => input.push_front(o),
                                    None => (),
                                }
                                input.push_front(TokenTree::Punct(p2));
                            }
                            Some(o) => input.push_front(o),
                            None => (),
                        }
                        input.push_front(TokenTree::Ident(id));
                    }
                    Some(o) => input.push_front(o),
                    None => (),
                }
                ret.extend(Some(TokenTree::Punct(p1)));
            }
            Some(TokenTree::Group(g)) => {
                let inner = remove_macro_arg_kind(g.stream());
                let mut g0 = Group::new(g.delimiter(), inner);
                g0.set_span(g.span());
                ret.extend(quote! { #g0 });
            }
            Some(o) => ret.extend(Some(o)),
            None => break,
        }
    }
    ret
}

fn emit_body(input: TokenStream) -> (TokenStream, bool) {
    let mut input: VecDeque<_> = input.into_iter().collect();
    let mut ret = TokenStream::new();
    let mut buf = TokenStream::new();
    let mut has_rec = false;
    loop {
        match input.pop_front() {
            Some(TokenTree::Punct(p1)) if p1.as_char() == '$' && p1.spacing() == Spacing::Alone => {
                match input.pop_front() {
                    Some(TokenTree::Ident(id)) if &id.to_string() == "self" => {
                        // Process $self!(...) recursive call
                        match input.pop_front() {
                            Some(TokenTree::Punct(p1))
                                if p1.as_char() == '!' && p1.spacing() == Spacing::Alone =>
                            {
                                match input.pop_front() {
                                    Some(TokenTree::Group(g)) => {
                                        if !buf.is_empty() {
                                            ret.extend(quote! { { #buf } });
                                            buf = TokenStream::new();
                                        }
                                        has_rec = true;
                                        ret.extend(quote! { call ( #{ g.stream() } ) });
                                        continue;
                                    }
                                    Some(o) => input.push_front(o),
                                    None => (),
                                }
                                input.push_front(TokenTree::Punct(p1));
                            }
                            Some(o) => input.push_front(o),
                            None => (),
                        }
                        input.push_front(TokenTree::Ident(id));
                    }
                    Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                        // Process $( ... ) in macro body
                        ret.extend(Some(TokenTree::Punct(p1)));
                        match input.pop_front() {
                            Some(TokenTree::Punct(p2))
                                if ['*', '+'].iter().any(|c| &p2.as_char() == c)
                                    && p2.spacing() == Spacing::Alone =>
                            {
                                // $ ( ... ) [*,+]
                                let (inner, flag) = emit_body(g.stream());
                                has_rec = has_rec || flag;
                                ret.extend(quote! { (#inner) #p2 });
                                // accept
                                continue;
                            }
                            Some(TokenTree::Punct(p2)) => {
                                match input.pop_front() {
                                    Some(TokenTree::Punct(p3))
                                        if ['*', '+'].iter().any(|c| &p3.as_char() == c)
                                            && p3.spacing() == Spacing::Alone =>
                                    {
                                        // $ ( ... ) SEP [*,+]
                                        let (inner, flag) = emit_body(g.stream());
                                        has_rec = has_rec || flag;
                                        ret.extend(quote! { (#inner) #p2 #p3});
                                        //accept
                                        continue;
                                    }
                                    Some(o) => {
                                        // Lexically illegal
                                        input.push_front(o);
                                    }
                                    None => (),
                                }
                                input.push_front(TokenTree::Punct(p2))
                            }
                            Some(o) => {
                                // Lexically illegal
                                input.push_front(o);
                            }
                            None => (),
                        }
                        input.push_front(TokenTree::Group(g));
                        continue;
                    }
                    Some(o) => input.push_front(o),
                    None => (),
                }
                buf.extend(Some(TokenTree::Punct(p1)));
            }
            Some(TokenTree::Group(g)) => {
                let (inner, flag) = emit_body(g.stream());
                has_rec = has_rec || flag;
                let close_id = match g.delimiter() {
                    Delimiter::Parenthesis => "parenthesis",
                    Delimiter::Brace => "brace",
                    Delimiter::Bracket => "bracket",
                    _ => abort!(g.span(), "Bad group"),
                };
                let close = Ident::new(close_id, Span::call_site());
                if !buf.is_empty() {
                    ret.extend(quote! { { #buf } });
                    buf = TokenStream::new();
                }
                ret.extend(quote! { group #inner #close });
            }
            Some(o) => buf.extend(Some(o)),
            None => {
                if !buf.is_empty() {
                    ret.extend(quote! { { #buf } });
                }
                break;
            }
        }
    }
    (ret, has_rec)
}

pub fn recursive(_attr: TokenStream, input: ItemMacro) -> TokenStream {
    if !input.mac.path.is_ident("macro_rules") {
        abort!(
            input.mac.path.span(),
            "Attribute #[recursive] is only accepted by 'macro_rules' macro declaration"
        )
    }
    let macro_name = if let Some(ident) = input.ident.clone() {
        ident
    } else {
        abort!(input.span(), "No name is given to the macro_rules! macro")
    };
    let internal_name = Ident::new(
        &format!("__internal_{}_{}", &macro_name.to_string(), getrandom()),
        macro_name.span(),
    );
    let body: MacroRulesBody = syn::parse2(input.mac.tokens.clone())
        .unwrap_or_else(|_| abort! {input.mac.span(), "Cannot parse body of macro_rules!"});

    let arms: Vec<(TokenStream, bool)> = body
        .0
        .iter()
        .map(|(arg, _, body, _)| {
            let (body_out, has_rec) = emit_body(body.stream());
            let emitted = quote! {
                (
                    @macro_rules_rep_internal
                    [ $($__mrp_stack_rem:tt)* ]
                    call (#{ arg.stream() })
                    $($__mrp_body_rem:tt)*
                ) => {
                    #internal_name!{
                        @macro_rules_rep_internal
                        [ $($__mrp_stack_rem)* ]
                            #body_out
                            $($__mrp_body_rem)*
                    }
                };
            };
            (emitted, has_rec)
        })
        .collect();

    quote! {
        #[doc(hidden)]
        #[macro_export]
        macro_rules! #internal_name {
            (
                @macro_rules_rep_internal
                [ // stack
                $($__mrp_stack_rem:tt)*
                ]
                group
                $($__mrp_body_rem:tt)*
            ) => {
                #internal_name!{
                    @macro_rules_rep_internal
                        [
                        []
                            $($__mrp_stack_rem)*
                        ]
                        $($__mrp_body_rem)*
                }
            };
            (
                @macro_rules_rep_internal
                [ // stack
                [$($__mrp_stack:tt)*]
                [$($__mrp_stack0:tt)*]
                $($__mrp_stack_rem:tt)*
                ]
                parenthesis
                $($__mrp_body_rem:tt)*
            ) => {
                #internal_name!{
                    @macro_rules_rep_internal
                        [
                        [$($__mrp_stack0)* ($($__mrp_stack)*)]
                            $($__mrp_stack_rem)*
                        ]
                        $($__mrp_body_rem)*
                }
            };
            (
                @macro_rules_rep_internal
                [ // stack
                [$($__mrp_stack:tt)*]
                [$($__mrp_stack0:tt)*]
                $($__mrp_stack_rem:tt)*
                ]
                brace
                $($__mrp_body_rem:tt)*
            ) => {
                #internal_name!{
                    @macro_rules_rep_internal
                        [
                        [$($__mrp_stack0)* {$($__mrp_stack)*}]
                            $($__mrp_stack_rem)*
                        ]
                        $($__mrp_body_rem)*
                }
            };
            (
                @macro_rules_rep_internal
                [ // stack
                [$($__mrp_stack:tt)*]
                [$($__mrp_stack0:tt)*]
                $($__mrp_stack_rem:tt)*
                ]
                bracket
                $($__mrp_body_rem:tt)*
            ) => {
                #internal_name!{
                    @macro_rules_rep_internal
                        [
                        [$($__mrp_stack0)* [$($__mrp_stack)*]]
                            $($__mrp_stack_rem)*
                        ]
                        $($__mrp_body_rem)*
                }
            };
            (
                @macro_rules_rep_internal
                [ // stack
                [$($__mrp_stack:tt)*]   // stack top
                $($__mrp_stack_rem:tt)*
                ]
                { $($__mrp_body_raw:tt)* }
                $($__mrp_body_rem:tt)*
            ) => {
                #internal_name!{
                    @macro_rules_rep_internal
                        [
                        [$($__mrp_stack)* $($__mrp_body_raw)*]
                            $($__mrp_stack_rem)*
                        ]
                        $($__mrp_body_rem)*
                }
            };
            #(for (arm, _) in arms.iter()) { #arm }
            (
                @macro_rules_rep_internal
                [ // stack
                [$($__mrp_stack:tt)*]   // stack top
                $($__mrp_stack_rem:tt)*
                ]
                $punct:tt
                $($__mrp_body_rem:tt)*
            ) => {
                #internal_name!{
                    @macro_rules_rep_internal
                        [
                        [$($__mrp_stack)* $punct]
                            $($__mrp_stack_rem)*
                        ]
                        $($__mrp_body_rem)*
                }
            };
            (
                @macro_rules_rep_internal
                [ // stack
                [$($__mrp_stack:tt)*]
                ]
            ) => {
                $($__mrp_stack)*
            };
        }

        #(let attrs = &input.attrs){ #(#attrs)* }
        #{ &input.mac.path } ! #macro_name {
            #(for ((_, has_rep), (arg, farr, body, bang)) in arms.iter().zip(&body.0)) {
                #(if !*has_rep) {
                    #arg #farr #body #bang
                }
                #(else) {
                    #arg #farr {
                        #internal_name!{
                            @macro_rules_rep_internal
                            [[]]
                                call (#{ remove_macro_arg_kind(arg.stream()) })
                        }
                    };
                }
            }

        }
    }
}
