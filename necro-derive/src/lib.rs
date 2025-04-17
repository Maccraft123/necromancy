use darling::{
    FromDeriveInput, FromMeta, FromVariant, FromField,
    FromAttributes, Error,
    util, ast,
};
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Ident};
use quote::{quote, format_ident};

#[derive(Debug, FromVariant)]
#[darling(attributes(operand))]
struct OperandVariant {
    ident: Ident,
    alias: Option<String>,
    rename: Option<String>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(operand), supports(enum_any))]
struct OperandInput {
    ident: Ident,
    data: ast::Data<OperandVariant, util::Ignored>,
}

fn ident_upper_lower(i: &Ident) -> (Ident, Ident) {
    let upper = i.to_string().to_uppercase();
    let lower = i.to_string().to_lowercase();
    (Ident::new(&upper, i.span()), Ident::new(&lower, i.span()))
}

#[proc_macro_derive(ParseOperand, attributes(operand))]
pub fn operand(input: TokenStream) -> TokenStream {
    let input = match OperandInput::from_derive_input(&parse_macro_input!(input as DeriveInput)) {
        Ok(v) => v,
        Err(e) => {
            return Error::from(e).write_errors().into();
        }
    };

    let variants = input.data.take_enum().unwrap();
    let mut parsers = Vec::new();

    /*
     * For every enum variant, get its name, and build a parser that will check if it recognizes
     * the variant name and constructs the enum variant if so
     */
    for var in variants {
        // ident is used for matching purposes
        // name is used for parsing
        let ident = var.ident;
        let name;
        if let Some(rename) = var.rename.as_ref() {
            name = Ident::new(rename, ident.span());
        } else {
            name = ident.clone();
        }

        let (upper, lower) = ident_upper_lower(&name);
        if let Some(alias) = var.alias.as_ref() {
            let alias = Ident::new(alias, name.span());
            let (a_upper, a_lower) = ident_upper_lower(&alias);
            parsers.push(
                quote! {
                    stringify!(#a_lower) => empty.value(Self::#ident),
                    stringify!(#a_upper) => empty.value(Self::#ident),
                }
            );
        }
        parsers.push(
            quote! {
                stringify!(#lower) => empty.value(Self::#ident),
                stringify!(#upper) => empty.value(Self::#ident),
            }
        );
    }

    let name = input.ident;
    quote! {
        impl<'a> ParseOperand<'a> for #name {
            fn parse(input: &mut crate::lexer::Stream<'a, '_>) -> winnow::ModalResult<#name> {
                use ::winnow::Parser;
                use ::winnow::combinator::{empty, fail};
                /*
                 * Here we grab all the parsers we've built, and throw them all into `dispatch`
                 * combinator to get any of those variants.
                 */
                winnow::combinator::dispatch!{crate::lexer::ident;
                    #(#parsers)*
                    _ => fail,
                }.parse_next(input)
            }
        }
    }.into()
}

#[derive(Debug, FromField)]
struct InstructionField {
    //ident: Option<syn::Ident>,
    ty: syn::Type,
}

#[derive(Debug, FromVariant)]
#[darling(attributes(parse))]
struct InstructionVariant {
    ident: Ident,
    #[darling(default)]
    fuse_first_field: bool,
    fields: ast::Fields<InstructionField>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(parse), supports(enum_any))]
struct InstructionInput {
    ident: Ident,
    generics: syn::Generics,
    data: ast::Data<InstructionVariant, util::Ignored>
}

fn make_field_parsers(input: impl IntoIterator<Item = InstructionField>, offset: isize) -> Vec<proc_macro2::TokenStream> {
    let mut ret = Vec::new();

    for (i, f) in input.into_iter().enumerate() {
        let ty = f.ty;
        let prefix = match offset + i as isize {
            isize::MIN..0 => quote! { empty, },
            0 => quote! { space1, },
            _ => quote! { (opt(','), space1), },
        };
        ret.push(quote! {
            preceded(
                #prefix
                <#ty as ParseOperand>::parse
            )
        });
    }

    ret
}

#[proc_macro_derive(ParseInstruction, attributes(parse))]
pub fn instruction(input: TokenStream) -> TokenStream {
    let input = match InstructionInput::from_derive_input(&parse_macro_input!(input as DeriveInput)) {
        Ok(v) => v,
        Err(e) => {
            return Error::from(e).write_errors().into();
        }
    };
    
    let variants = input.data.take_enum().unwrap();
    
    let mut variant_parsers = Vec::new();
    let mut fused_variants = Vec::new();
    for var in variants {
        let name = var.ident;
        let (name_upper, name_lower) = ident_upper_lower(&name);

        let mut field_parsers = Vec::new();
        let mut field_names = Vec::new();

        if var.fields.is_empty() {
            variant_parsers.push(quote! {
                stringify!(#name_upper) => empty.value(Self::#name),
                stringify!(#name_lower) => empty.value(Self::#name),
            });
        } else {
            let field_off = if var.fuse_first_field { -1 } else { 0 };
            field_parsers = make_field_parsers(var.fields, field_off);
            field_names = field_parsers
                .iter()
                .enumerate()
                .map(|(i, _)| format_ident!("field{i}"))
                .collect();
            let mut fields = quote!{ (#(#field_parsers),*).map(|(#(#field_names),*)| Self::#name(#(#field_names),*)) };

            if var.fuse_first_field {
                fused_variants.push(quote! {
                    preceded(stringify!(#name_upper), #fields),
                    preceded(stringify!(#name_lower), #fields),
                });
            } else {
                variant_parsers.push(quote! {
                    stringify!(#name_upper) => #fields,
                    stringify!(#name_lower) => #fields,
                });
            }
        }
    }

    let mut parser;
    if fused_variants.is_empty() {
        parser = quote!{
            winnow::combinator::dispatch!{crate::lexer::ident;
                #(#variant_parsers)*
                _ => fail,
            }.parse_next(input)
        };
    } else {
        parser = quote!{
            winnow::combinator::alt((
                winnow::combinator::dispatch!{crate::lexer::ident;
                    #(#variant_parsers)*
                    _ => fail,
                },
                #(#fused_variants)*
            )).parse_next(input)
        };
    }

    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    quote! {
        impl #impl_generics ParseInstruction<'a> for #name #ty_generics #where_clause {
            fn parse<'b>(input: &mut crate::lexer::Stream<'a, 'b>) -> winnow::ModalResult<#name #ty_generics> where Self: 'a
            {
                use ::winnow::Parser;
                use ::winnow::ascii::space1;
                use ::winnow::combinator::{
                    preceded, empty, fail, opt,
                };
                #parser
            }
        }
    }.into()
}

#[derive(Debug, FromVariant)]
struct EncodeOperandVariant {
    ident: Ident,
}

#[derive(Debug, FromDeriveInput)]
#[darling(supports(enum_any), forward_attrs(repr))]
struct EncodeOperandInput {
    ident: Ident,
    attrs: Vec<syn::Attribute>,
    data: ast::Data<EncodeOperandVariant, util::Ignored>
}

#[proc_macro_derive(EncodeOperand)]
pub fn encode_operand(input: TokenStream) -> TokenStream {
    let input = match EncodeOperandInput::from_derive_input(&parse_macro_input!(input as DeriveInput)) {
        Ok(v) => v,
        Err(e) => {
            return Error::from(e).write_errors().into();
        }
    };

    let primitive = input.attrs.iter()
        .flat_map(|i| darling::util::parse_attribute_to_meta_list(i))
        .find(|list| list.path.get_ident().is_some_and(|ident| ident.to_string() == "repr"))
        .map(|list| list.tokens);

    let name = input.ident;
    quote! {
        impl EncodeOperand<#primitive> for #name {
            fn encode_into(&self,
                target: &mut crate::Section,
                shift: u32,
                map: Option<fn(#name) -> #primitive>,
                _: &mut crate::SymbolRegistry,
            ) {
                let last = target.last_mut();
                if let Some(m) = map {
                    *last |= m(*self) << shift;
                } else {
                    *last |= (*self as #primitive) << shift;
                }
            }
        }
    }.into()
}

#[derive(Debug, FromField)]
#[darling(attributes(encode))]
struct EncodeInstructionField {
    #[darling(default)]
    shift: Option<syn::Expr>,
    #[darling(default)]
    with: Option<syn::Path>,
    #[darling(default)]
    map: Option<syn::Expr>,
}

#[derive(Debug, FromVariant)]
#[darling(attributes(encode))]
struct EncodeInstructionVariant {
    ident: Ident,
    fields: ast::Fields<EncodeInstructionField>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(supports(enum_any), attributes(encode), forward_attrs(repr))]
struct EncodeInstructionInput {
    ident: Ident,
    generics: syn::Generics,
    endian: String,
    attrs: Vec<syn::Attribute>,
    data: ast::Data<EncodeInstructionVariant, util::Ignored>
}

#[proc_macro_derive(EncodeInstruction, attributes(encode))]
pub fn encode_instruction(input: TokenStream) -> TokenStream {
    let input = match EncodeInstructionInput::from_derive_input(&parse_macro_input!(input as DeriveInput)) {
        Ok(v) => v,
        Err(e) => {
            return Error::from(e).write_errors().into();
        }
    };

    let primitive = input.attrs.iter()
        .flat_map(|i| darling::util::parse_attribute_to_meta_list(i))
        .find(|list| list.path.get_ident().is_some_and(|ident| ident.to_string() == "repr"))
        .map(|list| list.tokens)
        .unwrap();
    eprintln!("{}", primitive);
    
    let variants = input.data.take_enum().unwrap();

    let into_bytes = match input.endian.as_str() {
        "little" => quote! { to_le_bytes },
        "big" => quote! { to_be_bytes },
        _ => panic!(),
    };

    let mut arms = Vec::new();
    for var in variants.into_iter() {
        if var.fields.is_empty() {
            let ident = var.ident;
            arms.push(quote! {
                Self::#ident => {
                    out.extend(
                        (unsafe {
                            *(self as *const Self as *const #primitive)
                        })
                            .#into_bytes()
                            .into_iter()
                    );
                },
            });
        } else {
            let mut fields = Vec::new();
            let mut field_encoders = Vec::new();
            for (i, f) in var.fields.into_iter().enumerate() {
                let field = format_ident!("field{i}");
                fields.push(quote!{#field});

                let with;
                if let Some(m) = f.map {
                    with = quote! { Some(#m) };
                } else {
                    with = quote! { None };
                }

                let shift;
                if let Some(s) = f.shift {
                    shift = quote!{ #s };
                } else {
                    shift = quote!{ 0 };
                }

                if let Some(w) = f.with {
                    field_encoders.push(
                        quote!{
                            #w(*#field, out, reg);
                        }
                    );
                } else {
                    field_encoders.push(
                        quote!{
                            #field.encode_into(out, #shift, #with, reg);
                        }
                    );
                }
            }

            let ident = var.ident;
            arms.push(quote! {
                Self::#ident(#(#fields),*) => {
                    out.extend(
                        (unsafe {
                            *(self as *const Self as *const #primitive)
                        })
                            .#into_bytes()
                            .into_iter()
                    );
                    #(#field_encoders)*
                },
            });
        }
    }

    let name = input.ident;
    let (generics, ty_generics, where_clause) = input.generics.split_for_impl();
    quote! {
        impl #generics EncodeInstruction for #name #ty_generics #where_clause {
            fn encode(&self, out: &mut crate::Section, reg: &mut crate::SymbolRegistry) {
                match self {
                    #(#arms)*
                }
            }
        }
    }.into()
}
