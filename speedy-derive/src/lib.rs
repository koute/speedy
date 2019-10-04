#![recursion_limit="128"]

use std::collections::HashMap;
use std::u32;

extern crate proc_macro;
extern crate proc_macro2;

#[macro_use]
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;

trait IterExt: Iterator + Sized {
    fn collect_vec( self ) -> Vec< Self::Item > {
        self.collect()
    }
}

impl< T > IterExt for T where T: Iterator + Sized {}

#[proc_macro_derive(Readable, attributes(speedy))]
pub fn readable( input: proc_macro::TokenStream ) -> proc_macro::TokenStream {
    let input = parse_macro_input!( input as syn::DeriveInput );
    let tokens = impl_readable( input );
    proc_macro::TokenStream::from( tokens )
}

#[proc_macro_derive(Writable, attributes(speedy))]
pub fn writable( input: proc_macro::TokenStream ) -> proc_macro::TokenStream {
    let input = parse_macro_input!( input as syn::DeriveInput );
    let tokens = impl_writable( input );
    proc_macro::TokenStream::from( tokens )
}

#[derive(Copy, Clone, PartialEq)]
enum Variant {
    Readable,
    Writable
}

fn possibly_uses_generic_ty( generic_types: &[&syn::Ident], ty: &syn::Type ) -> bool {
    match ty {
        syn::Type::Path( syn::TypePath { qself: None, path: syn::Path { leading_colon: None, segments } } ) => {
            segments.iter().any( |segment| {
                if generic_types.iter().any( |&ident| ident == &segments[ 0 ].ident ) {
                    return true;
                }

                match segment.arguments {
                    syn::PathArguments::None => false,
                    syn::PathArguments::AngleBracketed( syn::AngleBracketedGenericArguments { ref args, .. } ) => {
                        args.iter().any( |arg| {
                            match arg {
                                syn::GenericArgument::Lifetime( .. ) => false,
                                syn::GenericArgument::Type( inner_ty ) => possibly_uses_generic_ty( generic_types, inner_ty ),
                                // TODO: How to handle these?
                                syn::GenericArgument::Binding( .. ) => true,
                                syn::GenericArgument::Constraint( .. ) => true,
                                syn::GenericArgument::Const( .. ) => true
                            }
                        })
                    },
                    _ => true
                }
            })
        },
        _ => true
    }
}

fn common_tokens( ast: &syn::DeriveInput, types: &[&syn::Type], variant: Variant ) -> (TokenStream, TokenStream, TokenStream) {
    let impl_params = {
        let lifetime_params = ast.generics.lifetimes().map( |alpha| quote! { #alpha } );
        let type_params = ast.generics.type_params().map( |ty| quote! { #ty } );
        let params = lifetime_params.chain( type_params ).collect_vec();
        quote! {
            #(#params,)*
        }
    };

    let ty_params = {
        let lifetime_params = ast.generics.lifetimes().map( |alpha| quote! { #alpha } );
        let type_params = ast.generics.type_params().map( |ty| { let ident = &ty.ident; quote! { #ident } } );
        let params = lifetime_params.chain( type_params ).collect_vec();
        if params.is_empty() {
            quote! {}
        } else {
            quote! { < #(#params),* > }
        }
    };

    let generics: Vec< _ > = ast.generics.type_params().map( |ty| &ty.ident ).collect();
    let where_clause = {
        let constraints = types.iter().filter_map( |&ty| {
            let possibly_generic = possibly_uses_generic_ty( &generics, ty );
            match (variant, possibly_generic) {
                (Variant::Readable, true) => Some( quote! { #ty: speedy::Readable< 'a_, C_ > } ),
                (Variant::Readable, false) => Some( quote! { #ty: 'a_ } ),
                (Variant::Writable, true) => Some( quote! { #ty: speedy::Writable< C_ > } ),
                (Variant::Writable, false) => None
            }
        });

        let mut predicates = Vec::new();
        if let Some( where_clause ) = ast.generics.where_clause.as_ref() {
            predicates = where_clause.predicates.iter().map( |pred| quote! { #pred } ).collect();
        }

        if variant == Variant::Readable {
            for lifetime in ast.generics.lifetimes() {
                predicates.push(
                    quote! { 'a_: #lifetime }
                );
            }
        }

        let items = constraints.chain( predicates.into_iter() ).collect_vec();
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
    ty: &'a syn::Type,
    default_on_eof: bool,
    count: Option< syn::Expr >
}

impl< 'a > Field< 'a > {
    fn var_name( &self ) -> syn::Ident {
        if let Some( name ) = self.name {
            name.clone()
        } else {
            syn::Ident::new( &format!( "t{}", self.index ), Span::call_site() )
        }
    }

    fn name( &self ) -> syn::Member {
        if let Some( name ) = self.name {
            syn::Member::Named( name.clone() )
        } else {
            syn::Member::Unnamed( syn::Index { index: self.index as u32, span: Span::call_site() } )
        }
    }
}

enum Attribute {
    DefaultOnEof,
    Count( syn::Expr )
}

impl syn::parse::Parse for Attribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let key: syn::Ident = input.parse()?;
        let value = match key.to_string().as_str() {
            "default_on_eof" => Attribute::DefaultOnEof,
            "count" => {
                let _: Token![=] = input.parse()?;
                let expr: syn::Expr = input.parse()?;
                Attribute::Count( expr )
            },
            key => panic!( "Unrecognized attribute: '{}'", key )
        };

        Ok( value )
    }
}

struct Attributes( syn::punctuated::Punctuated< Attribute, Token![,] > );

impl syn::parse::Parse for Attributes {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let content;
        parenthesized!( content in input );
        Ok( Attributes( content.parse_terminated( Attribute::parse )? ) )
    }
}

fn get_fields< 'a, I: IntoIterator< Item = &'a syn::Field > + 'a >( fields: I ) -> impl Iterator< Item = Field< 'a > > {
    let iter = fields.into_iter()
        .enumerate()
        .map( |(index, field)| {
            let mut default_on_eof = false;
            let mut count = None;
            for attr in &field.attrs {
                let path = attr.path.clone().into_token_stream().to_string();
                if path == "speedy" {
                    let parsed_attrs: Attributes = syn::parse2( attr.tokens.clone() ).unwrap();
                    for attr in parsed_attrs.0 {
                        match attr {
                            Attribute::DefaultOnEof => default_on_eof = true,
                            Attribute::Count( expr ) => count = Some( expr )
                        }
                    }
                }
            }

            if count.is_some() {
                let type_name = field.ty.clone().into_token_stream().to_string().replace( " ", "" );
                let is_vec = type_name.starts_with( "Vec<" );
                let is_cow_slice = type_name.starts_with( "Cow<" ) && type_name.ends_with( "]>" ) && type_name.contains( ",[" );
                if !is_vec && !is_cow_slice {
                    panic!( "The 'count' attribute is only supported for `Vec` or for `Cow<[_]>`" );
                }
            }

            Field {
                index,
                name: field.ident.as_ref(),
                ty: &field.ty,
                default_on_eof,
                count
            }
        });

    iter
}

fn readable_body< 'a, I >( types: &mut Vec< &'a syn::Type >, fields: I ) -> (TokenStream, TokenStream, TokenStream)
    where I: IntoIterator< Item = &'a syn::Field > + 'a
{
    let fields = fields.into_iter();
    let mut field_names = Vec::new();
    let mut field_readers = Vec::new();
    let mut minimum_bytes_needed = Vec::new();
    for field in get_fields( fields ) {
        let ident = field.var_name();
        types.push( field.ty );

        let body = if let Some( ref count ) = field.count {
            quote! { speedy::private::read_vec( _reader_, (#count) as usize ).map( |_value_| _value_.into() ) }
        } else {
            quote! { _reader_.read_value() }
        };

        let name = quote! { #ident };
        field_names.push( name );
        if field.default_on_eof {
            field_readers.push( quote! {
                let #ident = match #body {
                    Ok( value ) => value,
                    Err( ref error ) if error.kind() == std::io::ErrorKind::UnexpectedEof => std::default::Default::default(),
                    Err( error ) => return Err( error )
                };
            });
        } else {
            field_readers.push( quote! { let #ident = #body?; } );
        }

        if let Some( minimum_bytes ) = get_minimum_bytes( &field ) {
            minimum_bytes_needed.push( minimum_bytes );
        }
    }

    let body = quote! { #(#field_readers)* };
    let initializer = quote! { #(#field_names),* };
    let minimum_bytes_needed = sum( minimum_bytes_needed );
    (body, initializer, minimum_bytes_needed)
}

fn writable_body< 'a, I >( types: &mut Vec< &'a syn::Type >, fields: I ) -> (TokenStream, TokenStream)
    where I: IntoIterator< Item = Field< 'a > > + 'a
{
    let fields = fields.into_iter();
    let mut field_names = Vec::new();
    let mut field_writers = Vec::new();
    for field in fields {
        types.push( field.ty );

        let var_name = field.var_name();
        field_names.push( var_name.clone() );
        let expr = quote! { #var_name };

        let body = if let Some( count ) = field.count {
            let error_message = format!( "the length of '{}' is not the same as its 'count' attribute", var_name );
            quote! {
                let __expected = #count;
                if #expr.len() != __expected as usize {
                    return Err( std::io::Error::new( std::io::ErrorKind::InvalidData, #error_message ) );
                }
                speedy::private::write_slice( &#expr, _writer_ )?;
            }
        } else {
            quote! { _writer_.write_value( #expr )?; }
        };

        field_writers.push( body );
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
            Some( (_, syn::Expr::Lit( syn::ExprLit { lit: syn::Lit::Int( ref value ), .. } )) ) => {
                let value = value.base10_parse::< u64 >().unwrap();
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

fn get_minimum_bytes( field: &Field ) -> Option< TokenStream > {
    if field.default_on_eof {
        None
    } else {
        let ty = &field.ty;
        Some( quote! { <#ty as speedy::Readable< 'a_, C_ >>::minimum_bytes_needed() } )
    }
}

fn sum< I >( values: I ) -> TokenStream where I: IntoIterator< Item = TokenStream >, <I as IntoIterator>::IntoIter: ExactSizeIterator {
    let iter = values.into_iter();
    if iter.len() == 0 {
        quote! { 0 }
    } else {
        quote! {{
            let mut out = 0;
            #(out += #iter;)*
            out
        }}
    }
}

fn min< I >( values: I ) -> TokenStream where I: IntoIterator< Item = TokenStream >, <I as IntoIterator>::IntoIter: ExactSizeIterator {
    let iter = values.into_iter();
    if iter.len() == 0 {
        quote! { 0 }
    } else {
        quote! {{
            let mut out = 0;
            #(out = std::cmp::min( out, #iter );)*
            out
        }}
    }
}

fn impl_readable( input: syn::DeriveInput ) -> TokenStream {
    let name = &input.ident;
    let mut types = Vec::new();
    let (reader_body, minimum_bytes_needed_body) = match &input.data {
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Named( syn::FieldsNamed { named, .. } ), .. } ) => {
            let (body, initializer, minimum_bytes) = readable_body( &mut types, named );
            let reader_body = quote! {
                #body
                Ok( #name { #initializer } )
            };
            (reader_body, minimum_bytes)
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unnamed( syn::FieldsUnnamed { unnamed, .. } ), .. } ) => {
            let (body, initializer, minimum_bytes) = readable_body( &mut types, unnamed );
            let reader_body = quote! {
                #body
                Ok( #name( #initializer ) )
            };
            (reader_body, minimum_bytes)
        },
        syn::Data::Enum( syn::DataEnum { variants, .. } ) => {
            let mut ctx = EnumCtx::new( &name );
            let mut variant_matches = Vec::with_capacity( variants.len() );
            let mut variant_minimum_sizes = Vec::with_capacity( variants.len() );
            variants.iter()
                .for_each( |variant| {
                    let kind = ctx.next( &variant );
                    let unqualified_ident = &variant.ident;
                    let variant_path = quote! { #name::#unqualified_ident };

                    match variant.fields {
                        syn::Fields::Named( syn::FieldsNamed { ref named, .. } ) => {
                            let (body, initializer, minimum_bytes) = readable_body( &mut types, named );
                            variant_matches.push( quote! {
                                #kind => {
                                    #body
                                    Ok( #variant_path { #initializer } )
                                }
                            });
                            variant_minimum_sizes.push( minimum_bytes );
                        },
                        syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ) => {
                            let (body, initializer, minimum_bytes) = readable_body( &mut types, unnamed );
                            variant_matches.push( quote! {
                                #kind => {
                                    #body
                                    Ok( #variant_path( #initializer ) )
                                }
                            });
                            variant_minimum_sizes.push( minimum_bytes );
                        },
                        syn::Fields::Unit => {
                            variant_matches.push( quote! {
                                #kind => {
                                    Ok( #variant_path )
                                }
                            });
                        }
                    }
                });

            let reader_body = quote! {
                let kind_: u32 = _reader_.read_value()?;
                match kind_ {
                    #(#variant_matches),*
                    _ => Err( std::io::Error::new( std::io::ErrorKind::InvalidData, "invalid enum variant" ) )
                }
            };
            let minimum_bytes_needed_body = min( variant_minimum_sizes.into_iter() );
            let minimum_bytes_needed_body = quote! { (#minimum_bytes_needed_body) + 4 }; // For the tag.
            (reader_body, minimum_bytes_needed_body)
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unit, .. } ) => {
            let reader_body = quote! {
                Ok( #name )
            };

            let minimum_bytes_needed_body = quote! { 0 };
            (reader_body, minimum_bytes_needed_body)
        },
        syn::Data::Union( .. ) => panic!( "Unions are not supported!" )
    };

    let (impl_params, ty_params, where_clause) = common_tokens( &input, &types, Variant::Readable );
    quote! {
        impl< 'a_, #impl_params C_: speedy::Context > speedy::Readable< 'a_, C_ > for #name #ty_params #where_clause {
            #[inline]
            fn read_from< R_: speedy::Reader< 'a_, C_ > >( _reader_: &mut R_ ) -> std::io::Result< Self > {
                #reader_body
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                #minimum_bytes_needed_body
            }
        }
    }
}

fn assign_to_variables< 'a >( fields: impl IntoIterator< Item = &'a Field< 'a > > ) -> TokenStream {
    let fields: Vec< _ > = fields.into_iter().map( |field| {
        let var_name = field.var_name();
        let name = field.name();

        quote! {
            let #var_name = &self.#name;
        }
    }).collect();

    quote! {
        #(#fields)*
    }
}

fn impl_writable( input: syn::DeriveInput ) -> TokenStream {
    let name = &input.ident;
    let mut types = Vec::new();
    let writer_body = match input.data {
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unit, .. } ) => {
            quote! {}
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Named( syn::FieldsNamed { ref named, .. } ), .. } ) => {
            let fields: Vec< _ > = get_fields( named ).collect();
            let assignments = assign_to_variables( &fields );
            let (body, _) = writable_body( &mut types, fields );
            quote! {
                #assignments
                #body
            }
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ), .. } ) => {
            let fields: Vec< _ > = get_fields( unnamed ).collect();
            let assignments = assign_to_variables( &fields );
            let (body, _) = writable_body( &mut types, fields );
            quote! {
                #assignments
                #body
            }
        },
        syn::Data::Enum( syn::DataEnum { ref variants, .. } ) => {
            let mut ctx = EnumCtx::new( &name );
            let variants = variants.iter()
                .map( |variant| {
                    let kind = ctx.next( &variant );
                    let unqualified_ident = &variant.ident;
                    let variant_path = quote! { #name::#unqualified_ident };
                    match variant.fields {
                        syn::Fields::Named( syn::FieldsNamed { ref named, .. } ) => {
                            let (body, identifiers) = writable_body( &mut types, get_fields( named ) );
                            quote! {
                                #variant_path { #identifiers } => {
                                    _writer_.write_value( &#kind )?;
                                    #body
                                }
                            }
                        },
                        syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ) => {
                            let (body, identifiers) = writable_body( &mut types, get_fields( unnamed ) );
                            quote! {
                                #variant_path( #identifiers ) => {
                                    _writer_.write_value( &#kind )?;
                                    #body
                                }
                            }
                        },
                        syn::Fields::Unit => {
                            quote! { #variant_path => {
                                _writer_.write_value( &#kind )?;
                            }}
                        },
                    }
                })
                .collect_vec();
            quote! { match *self { #(#variants),* } }
        },
        syn::Data::Union( .. ) => panic!( "Unions are not supported!" )
    };

    let (impl_params, ty_params, where_clause) = common_tokens( &input, &types, Variant::Writable );
    quote! {
        impl< #impl_params C_: speedy::Context > speedy::Writable< C_ > for #name #ty_params #where_clause {
            #[inline]
            fn write_to< 'a_, T_: ?Sized + speedy::Writer< 'a_, C_ > >( &'a_ self, _writer_: &mut T_ ) -> std::io::Result< () > {
                #writer_body
                Ok(())
            }
        }
    }
}
