#![recursion_limit="128"]

use std::collections::HashMap;
use std::u32;

extern crate proc_macro;
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

trait IterExt: Iterator + Sized {
    fn collect_vec( self ) -> Vec< Self::Item > {
        self.collect()
    }
}

impl< T > IterExt for T where T: Iterator + Sized {}

#[proc_macro_derive(Readable)]
pub fn readable( input: TokenStream ) -> TokenStream {
    let src = input.to_string();
    let ast = syn::parse_macro_input( &src ).unwrap();
    let tokens = impl_readable( &ast );
    tokens.parse().unwrap()
}

#[proc_macro_derive(Writable)]
pub fn writable( input: TokenStream ) -> TokenStream {
    let src = input.to_string();
    let ast = syn::parse_macro_input( &src ).unwrap();
    let tokens = impl_writable( &ast );
    tokens.parse().unwrap()
}

enum Variant {
    Readable,
    Writable
}

fn common_tokens( ast: &syn::MacroInput, types: &[&syn::Ty], variant: Variant ) -> (quote::Tokens, quote::Tokens, quote::Tokens) {
    let impl_params = {
        let lifetime_params = ast.generics.lifetimes.iter().map( |alpha| quote! { #alpha } );
        let type_params = ast.generics.ty_params.iter().map( |ty| quote! { #ty } );
        let params = lifetime_params.chain( type_params ).collect_vec();
        quote! {
            #(#params,)*
        }
    };

    let ty_params = {
        let lifetime_params = ast.generics.lifetimes.iter().map( |alpha| quote! { #alpha } );
        let type_params = ast.generics.ty_params.iter().map( |ty| { let ident = &ty.ident; quote! { #ident } } );
        let params = lifetime_params.chain( type_params ).collect_vec();
        if params.is_empty() {
            quote! {}
        } else {
            quote! { < #(#params),* > }
        }
    };

    let where_clause = {
        let constraints = types.iter().map( |&ty| {
            match variant {
                Variant::Readable => quote! { #ty: ::speedy::Readable< 'a_, C_ > },
                Variant::Writable => quote! { #ty: ::speedy::Writable< C_ > }
            }
        });

        let predicates = ast.generics.where_clause.predicates.iter().map( |pred| quote! { #pred } );
        let items = constraints.chain( predicates ).collect_vec();
        if items.is_empty() {
            quote! {}
        } else {
            quote! { where #(#items),* }
        }
    };

    (impl_params, ty_params, where_clause)
}

struct Field< 'a > {
    index: usize,
    name: Option< &'a syn::Ident >,
    ty: &'a syn::Ty
}

impl< 'a > Field< 'a > {
    fn var_name( &self ) -> syn::Ident {
        if let Some( name ) = self.name {
            name.clone()
        } else {
            syn::Ident::from( format!( "v{}_", self.index ) )
        }
    }

    fn name( &self ) -> syn::Ident {
        if let Some( name ) = self.name {
            name.clone()
        } else {
            syn::Ident::from( format!( "{}", self.index ) )
        }
    }
}

fn get_fields< 'a >( fields: &'a [syn::Field] ) -> Box< Iterator< Item = Field< 'a > > + 'a > {
    let iter = fields.iter()
        .enumerate()
        .map( |(index, field)| {
            Field {
                index,
                name: field.ident.as_ref(),
                ty: &field.ty
            }
        });

    Box::new( iter )
}

fn readable_body< 'a >( types: &mut Vec< &'a syn::Ty >, fields: &'a [syn::Field] ) -> (quote::Tokens, quote::Tokens) {
    let mut field_names = Vec::new();
    let mut field_readers = Vec::new();
    for field in get_fields( &fields ) {
        let ident = field.var_name();
        types.push( field.ty );

        let name = quote! { #ident };
        field_names.push( name );
        field_readers.push( quote! { let #ident = _reader_.read_value()?; } )
    }

    let body = quote! { #(#field_readers)* };
    let initializer = quote! { #(#field_names),* };
    (body, initializer)
}

fn writable_body< 'a >( types: &mut Vec< &'a syn::Ty >, fields: &'a [syn::Field], is_unpacked: bool ) -> (quote::Tokens, quote::Tokens) {
    let mut field_names = Vec::new();
    let mut field_writers = Vec::new();
    for field in get_fields( &fields ) {
        types.push( field.ty );

        let reference = if is_unpacked {
            let name = field.var_name();
            field_names.push( name.clone() );

            quote! { #name }
        } else {
            let name = field.name();
            quote! { &self.#name }
        };

        field_writers.push( quote! { _writer_.write_value( #reference )?; } );
    }

    let body = quote! { #(#field_writers)* };
    let initializer = quote! { #(ref #field_names),* };
    (body, initializer)
}

struct EnumCtx {
    ident: syn::Ident,
    previous_kind: Option< u32 >,
    kind_to_full_name: HashMap< u32, String >
}

impl EnumCtx {
    fn new( ident: &syn::Ident ) -> Self {
        EnumCtx {
            ident: ident.clone(),
            previous_kind: None,
            kind_to_full_name: HashMap::new()
        }
    }

    fn next( &mut self, variant: &syn::Variant ) -> u32 {
        let full_name = format!( "{}::{}", self.ident, variant.ident );
        let kind = match variant.discriminant {
            None => {
                let kind = if let Some( previous_kind ) = self.previous_kind {
                    if previous_kind >= u32::MAX {
                        panic!( "Enum discriminant `{}` is too big!", full_name );
                    }

                    previous_kind + 1
                } else {
                    0
                };

                self.previous_kind = Some( kind );
                kind
            },
            Some( syn::ConstExpr::Lit( syn::Lit::Int( value, _ ) ) ) => {
                if value > u32::MAX as u64 {
                    panic!( "Enum discriminant `{}` is too big!", full_name );
                }

                let kind = value as u32;
                self.previous_kind = Some( kind );
                kind
            },
            _ => panic!( "Enum discriminant `{}` is currently unsupported!", full_name )
        };

        if let Some( other_full_name ) = self.kind_to_full_name.get( &kind ) {
            panic!( "Two discriminants with the same value of '{}': `{}`, `{}`", kind, full_name, other_full_name );
        }

        self.kind_to_full_name.insert( kind, full_name );
        kind
    }
}

fn impl_readable( ast: &syn::MacroInput ) -> quote::Tokens {
    let name = &ast.ident;
    let mut types = Vec::new();
    let reader_body = match ast.body {
        syn::Body::Struct( syn::VariantData::Struct( ref fields ) ) => {
            let (body, initializer) = readable_body( &mut types, fields );
            quote! {
                #body
                Ok( #name { #initializer } )
            }
        },
        syn::Body::Struct( syn::VariantData::Tuple( ref fields ) ) => {
            let (body, initializer) = readable_body( &mut types, fields );
            quote! {
                #body
                Ok( #name( #initializer ) )
            }
        },
        syn::Body::Enum( ref body ) => {
            let mut ctx = EnumCtx::new( &name );
            let variants = body.iter()
                .map( |variant| {
                    let kind = ctx.next( &variant );
                    let unqualified_ident = &variant.ident;
                    let variant_path = quote! { #name::#unqualified_ident };
                    match variant.data {
                        syn::VariantData::Struct( ref fields ) => {
                            let (body, initializer) = readable_body( &mut types, fields );
                            quote! {
                                #kind => {
                                    #body
                                    Ok( #variant_path { #initializer } )
                                }
                            }
                        },
                        syn::VariantData::Tuple( ref fields ) => {
                            let (body, initializer) = readable_body( &mut types, fields );
                            quote! {
                                #kind => {
                                    #body
                                    Ok( #variant_path( #initializer ) )
                                }
                            }
                        },
                        syn::VariantData::Unit => {
                            quote! { #kind => {
                                Ok( #variant_path )
                            }}
                        }
                    }
                })
                .collect_vec();

            quote! {
                let kind_: u32 = _reader_.read_value()?;
                match kind_ {
                    #(#variants),*
                    _ => Err( ::std::io::Error::new( ::std::io::ErrorKind::InvalidData, "invalid enum variant" ) )
                }
            }
        },
        syn::Body::Struct( syn::VariantData::Unit ) => {
            quote! {
                Ok( #name )
            }
        }
    };

    let (impl_params, ty_params, where_clause) = common_tokens( ast, &types, Variant::Readable );
    quote! {
        impl< 'a_, #impl_params C_: ::speedy::Context > ::speedy::Readable< 'a_, C_ > for #name #ty_params #where_clause {
            #[inline]
            fn read_from< R_: ::speedy::Reader< 'a_, C_ > >( _reader_: &mut R_ ) -> ::std::io::Result< Self > {
                #reader_body
            }
        }
    }
}

fn impl_writable( ast: &syn::MacroInput ) -> quote::Tokens {
    let name = &ast.ident;
    let mut types = Vec::new();
    let writer_body = match ast.body {
        syn::Body::Struct( syn::VariantData::Unit ) => {
            quote! {}
        },
        syn::Body::Struct( syn::VariantData::Struct( ref fields ) ) => {
            let (body, _) = writable_body( &mut types, fields, false );
            quote! { #body }
        },
        syn::Body::Struct( syn::VariantData::Tuple( ref fields ) ) => {
            let (body, _) = writable_body( &mut types, fields, false );
            quote! { #body }
        },
        syn::Body::Enum( ref variants ) => {
            let mut ctx = EnumCtx::new( &name );
            let variants = variants.iter()
                .map( |variant| {
                    let kind = ctx.next( &variant );
                    let unqualified_ident = &variant.ident;
                    let variant_path = quote! { #name::#unqualified_ident };
                    match variant.data {
                        syn::VariantData::Struct( ref fields ) => {
                            let (body, identifiers) = writable_body( &mut types, fields, true );
                            quote! {
                                #variant_path { #identifiers } => {
                                    _writer_.write_value( &#kind )?;
                                    #body
                                }
                            }
                        },
                        syn::VariantData::Tuple( ref fields ) => {
                            let (body, identifiers) = writable_body( &mut types, fields, true );
                            quote! {
                                #variant_path( #identifiers ) => {
                                    _writer_.write_value( &#kind )?;
                                    #body
                                }
                            }
                        },
                        syn::VariantData::Unit => {
                            quote! { #variant_path => {
                                _writer_.write_value( &#kind )?;
                            }}
                        },
                    }
                })
                .collect_vec();
            quote! { match *self { #(#variants),* } }
        }
    };

    let (impl_params, ty_params, where_clause) = common_tokens( ast, &types, Variant::Writable );
    quote! {
        impl< #impl_params C_: ::speedy::Context > ::speedy::Writable< C_ > for #name #ty_params #where_clause {
            #[inline]
            fn write_to< 'a_, T_: ?Sized + ::speedy::Writer< 'a_, C_ > >( &'a_ self, _writer_: &mut T_ ) -> ::std::io::Result< () > {
                #writer_body
                Ok(())
            }
        }
    }
}
