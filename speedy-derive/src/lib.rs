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

enum Variant {
    Readable,
    Writable
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

    let where_clause = {
        let constraints = types.iter().map( |&ty| {
            match variant {
                Variant::Readable => quote! { #ty: ::speedy::Readable< 'a_, C_ > },
                Variant::Writable => quote! { #ty: ::speedy::Writable< C_ > }
            }
        });

        let mut predicates = Vec::new();
        if let Some( where_clause ) = ast.generics.where_clause.as_ref() {
            predicates = where_clause.predicates.iter().map( |pred| quote! { #pred } ).collect();
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
    default_on_eof: bool
}

impl< 'a > Field< 'a > {
    fn var_name( &self ) -> syn::Ident {
        if let Some( name ) = self.name {
            name.clone()
        } else {
            syn::Ident::new( &format!( "v{}_", self.index ), Span::call_site() )
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

fn get_fields< 'a, I: IntoIterator< Item = &'a syn::Field > + 'a >( fields: I ) -> Box< Iterator< Item = Field< 'a > > + 'a > {
    let iter = fields.into_iter()
        .enumerate()
        .map( |(index, field)| {
            let mut default_on_eof = false;
            for attr in &field.attrs {
                match attr.parse_meta().expect( "unable to parse attribute" ) {
                    syn::Meta::List( syn::MetaList { ref ident, ref nested, .. } ) if ident == "speedy" => {
                        let nested: Vec< _ > = nested.iter().collect();
                        match &nested[..] {
                            [syn::NestedMeta::Meta( syn::Meta::Word( ident ) )] if ident == "default_on_eof" => {
                                default_on_eof = true;
                            },
                            _ => panic!( "Unrecognized attribute: {:?}", attr )
                        }
                    },
                    _ => {}
                }
            }

            Field {
                index,
                name: field.ident.as_ref(),
                ty: &field.ty,
                default_on_eof
            }
        });

    Box::new( iter )
}

fn readable_body< 'a, I >( types: &mut Vec< &'a syn::Type >, fields: I ) -> (TokenStream, TokenStream)
    where I: IntoIterator< Item = &'a syn::Field > + 'a
{
    let fields = fields.into_iter();
    let mut field_names = Vec::new();
    let mut field_readers = Vec::new();
    for field in get_fields( fields ) {
        let ident = field.var_name();
        types.push( field.ty );

        let name = quote! { #ident };
        field_names.push( name );
        if field.default_on_eof {
            field_readers.push( quote! {
                let #ident = match _reader_.read_value() {
                    Ok( value ) => value,
                    Err( ref error ) if error.kind() == ::std::io::ErrorKind::UnexpectedEof => ::std::default::Default::default(),
                    Err( error ) => return Err( error )
                };
            });
        } else {
            field_readers.push( quote! { let #ident = _reader_.read_value()?; } );
        }
    }

    let body = quote! { #(#field_readers)* };
    let initializer = quote! { #(#field_names),* };
    (body, initializer)
}

fn writable_body< 'a, I >( types: &mut Vec< &'a syn::Type >, fields: I, is_unpacked: bool ) -> (TokenStream, TokenStream)
    where I: IntoIterator< Item = &'a syn::Field > + 'a
{
    let fields = fields.into_iter();
    let mut field_names = Vec::new();
    let mut field_writers = Vec::new();
    for field in get_fields( fields ) {
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
            Some( (_, syn::Expr::Lit( syn::ExprLit { lit: syn::Lit::Int( ref value ), .. } )) ) => {
                let value = value.value();
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

fn impl_readable( input: syn::DeriveInput ) -> TokenStream {
    let name = &input.ident;
    let mut types = Vec::new();
    let reader_body = match &input.data {
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Named( syn::FieldsNamed { named, .. } ), .. } ) => {
            let (body, initializer) = readable_body( &mut types, named );
            quote! {
                #body
                Ok( #name { #initializer } )
            }
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unnamed( syn::FieldsUnnamed { unnamed, .. } ), .. } ) => {
            let (body, initializer) = readable_body( &mut types, unnamed );
            quote! {
                #body
                Ok( #name( #initializer ) )
            }
        },
        syn::Data::Enum( syn::DataEnum { variants, .. } ) => {
            let mut ctx = EnumCtx::new( &name );
            let variants = variants.iter()
                .map( |variant| {
                    let kind = ctx.next( &variant );
                    let unqualified_ident = &variant.ident;
                    let variant_path = quote! { #name::#unqualified_ident };
                    match variant.fields {
                        syn::Fields::Named( syn::FieldsNamed { ref named, .. } ) => {
                            let (body, initializer) = readable_body( &mut types, named );
                            quote! {
                                #kind => {
                                    #body
                                    Ok( #variant_path { #initializer } )
                                }
                            }
                        },
                        syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ) => {
                            let (body, initializer) = readable_body( &mut types, unnamed );
                            quote! {
                                #kind => {
                                    #body
                                    Ok( #variant_path( #initializer ) )
                                }
                            }
                        },
                        syn::Fields::Unit => {
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
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unit, .. } ) => {
            quote! {
                Ok( #name )
            }
        },
        syn::Data::Union( .. ) => panic!( "Unions are not supported!" )
    };

    let (impl_params, ty_params, where_clause) = common_tokens( &input, &types, Variant::Readable );
    quote! {
        impl< 'a_, #impl_params C_: ::speedy::Context > ::speedy::Readable< 'a_, C_ > for #name #ty_params #where_clause {
            #[inline]
            fn read_from< R_: ::speedy::Reader< 'a_, C_ > >( _reader_: &mut R_ ) -> ::std::io::Result< Self > {
                #reader_body
            }
        }
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
            let (body, _) = writable_body( &mut types, named, false );
            quote! { #body }
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ), .. } ) => {
            let (body, _) = writable_body( &mut types, unnamed, false );
            quote! { #body }
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
                            let (body, identifiers) = writable_body( &mut types, named, true );
                            quote! {
                                #variant_path { #identifiers } => {
                                    _writer_.write_value( &#kind )?;
                                    #body
                                }
                            }
                        },
                        syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ) => {
                            let (body, identifiers) = writable_body( &mut types, unnamed, true );
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
        impl< #impl_params C_: ::speedy::Context > ::speedy::Writable< C_ > for #name #ty_params #where_clause {
            #[inline]
            fn write_to< 'a_, T_: ?Sized + ::speedy::Writer< 'a_, C_ > >( &'a_ self, _writer_: &mut T_ ) -> ::std::io::Result< () > {
                #writer_body
                Ok(())
            }
        }
    }
}
