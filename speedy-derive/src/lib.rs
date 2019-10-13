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
use syn::spanned::Spanned;

trait IterExt: Iterator + Sized {
    fn collect_vec( self ) -> Vec< Self::Item > {
        self.collect()
    }
}

impl< T > IterExt for T where T: Iterator + Sized {}

#[proc_macro_derive(Readable, attributes(speedy))]
pub fn readable( input: proc_macro::TokenStream ) -> proc_macro::TokenStream {
    let input = parse_macro_input!( input as syn::DeriveInput );
    let tokens = impl_readable( input ).unwrap_or_else( |err| err.to_compile_error() );
    proc_macro::TokenStream::from( tokens )
}

#[proc_macro_derive(Writable, attributes(speedy))]
pub fn writable( input: proc_macro::TokenStream ) -> proc_macro::TokenStream {
    let input = parse_macro_input!( input as syn::DeriveInput );
    let tokens = impl_writable( input ).unwrap_or_else( |err| err.to_compile_error() );
    proc_macro::TokenStream::from( tokens )
}

mod kw {
    syn::custom_keyword!( count );
    syn::custom_keyword!( default_on_eof );
    syn::custom_keyword!( tag_type );

    syn::custom_keyword!( u8 );
    syn::custom_keyword!( u16 );
    syn::custom_keyword!( u32 );
    syn::custom_keyword!( u64 );
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
                (Variant::Readable, false) => None,
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
                predicates.push( quote! { 'a_: #lifetime } );
                predicates.push( quote! { #lifetime: 'a_ } );
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

enum ItemKind {
    Struct,
    Enum
}

enum BasicType {
    U8,
    U16,
    U32,
    U64
}

enum ItemAttribute {
    TagType {
        key_token: kw::tag_type,
        ty: BasicType
    }
}

impl syn::parse::Parse for ItemAttribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        let value = if lookahead.peek( kw::tag_type ) {
            let key_token = input.parse::< kw::tag_type >()?;
            let _: Token![=] = input.parse()?;
            let lookahead = input.lookahead1();
            let ty = if lookahead.peek( kw::u8 ) {
                input.parse::< kw::u8 >()?;
                BasicType::U8
            } else if lookahead.peek( kw::u16 ) {
                input.parse::< kw::u16 >()?;
                BasicType::U16
            } else if lookahead.peek( kw::u32 ) {
                input.parse::< kw::u32 >()?;
                BasicType::U32
            } else if lookahead.peek( kw::u64 ) {
                input.parse::< kw::u64 >()?;
                BasicType::U64
            } else {
                return Err( lookahead.error() );
            };

            ItemAttribute::TagType {
                key_token,
                ty
            }
        } else {
            return Err( lookahead.error() )
        };

        Ok( value )
    }
}

struct RawItemAttributes( syn::punctuated::Punctuated< ItemAttribute, Token![,] > );

impl syn::parse::Parse for RawItemAttributes {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let content;
        parenthesized!( content in input );
        Ok( RawItemAttributes( content.parse_terminated( ItemAttribute::parse )? ) )
    }
}

struct ItemAttributes {
    tag_type: Option< BasicType >
}

fn parse_item_attributes( kind: ItemKind, attrs: &[syn::Attribute] ) -> Result< ItemAttributes, syn::Error > {
    let mut tag_type = None;
    for raw_attr in attrs {
        let path = raw_attr.path.clone().into_token_stream().to_string();
        if path != "speedy" {
            continue;
        }

        let parsed_attrs: RawItemAttributes = syn::parse2( raw_attr.tokens.clone() )?;
        for attr in parsed_attrs.0 {
            match attr {
                ItemAttribute::TagType { key_token, ty } => {
                    if tag_type.is_some() {
                        let message = "Duplicate 'tag_type'";
                        return Err( syn::Error::new( key_token.span(), message ) );
                    }
                    match kind {
                        ItemKind::Enum => {},
                        ItemKind::Struct => {
                            let message = "The 'tag_type' attribute can only be used on enums";
                            return Err( syn::Error::new( key_token.span(), message ) );
                        }
                    }

                    tag_type = Some( ty );
                }
            }
        }
    }

    Ok( ItemAttributes {
        tag_type
    })
}

struct Struct< 'a > {
    fields: Vec< Field< 'a > >
}

impl< 'a > Struct< 'a > {
    fn new( fields: impl IntoIterator< Item = &'a syn::Field > + 'a ) -> Result< Self, syn::Error > {
        let fields = get_fields( fields.into_iter() )?;
        Ok( Struct {
            fields
        })
    }
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

enum FieldAttribute {
    DefaultOnEof,
    Count( syn::Expr )
}

impl syn::parse::Parse for FieldAttribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        let value = if lookahead.peek( kw::default_on_eof ) {
            input.parse::< kw::default_on_eof >()?;
            FieldAttribute::DefaultOnEof
        } else if lookahead.peek( kw::count ) {
            input.parse::< kw::count >()?;
            let _: Token![=] = input.parse()?;
            let expr: syn::Expr = input.parse()?;
            FieldAttribute::Count( expr )
        } else {
            return Err( lookahead.error() )
        };

        Ok( value )
    }
}

struct FieldAttributes( syn::punctuated::Punctuated< FieldAttribute, Token![,] > );

impl syn::parse::Parse for FieldAttributes {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let content;
        parenthesized!( content in input );
        Ok( FieldAttributes( content.parse_terminated( FieldAttribute::parse )? ) )
    }
}

fn get_fields< 'a, I: IntoIterator< Item = &'a syn::Field > + 'a >( fields: I ) -> Result< Vec< Field< 'a > >, syn::Error > {
    let iter = fields.into_iter()
        .enumerate()
        .map( |(index, field)| {
            let mut default_on_eof = false;
            let mut count = None;
            for attr in &field.attrs {
                let path = attr.path.clone().into_token_stream().to_string();
                if path == "speedy" {
                    let parsed_attrs: FieldAttributes = syn::parse2( attr.tokens.clone() )?;
                    for attr in parsed_attrs.0 {
                        match attr {
                            FieldAttribute::DefaultOnEof => default_on_eof = true,
                            FieldAttribute::Count( expr ) => count = Some( expr ),
                        }
                    }
                }
            }

            if count.is_some() {
                let type_name = field.ty.clone().into_token_stream().to_string().replace( " ", "" );
                let is_vec = type_name.starts_with( "Vec<" );
                let is_cow_slice = type_name.starts_with( "Cow<" ) && type_name.ends_with( "]>" ) && type_name.contains( ",[" );
                if !is_vec && !is_cow_slice {
                    return Err( syn::Error::new( field.ty.span(), "The 'count' attribute is only supported for `Vec` or for `Cow<[_]>`" ) );
                }
            }

            Ok( Field {
                index,
                name: field.ident.as_ref(),
                ty: &field.ty,
                default_on_eof,
                count
            })
        });

    iter.collect()
}

fn read_field_body( count: Option< syn::Expr >, default_on_eof: bool ) -> TokenStream {
    let body = if let Some( ref count ) = count {
        quote! { _reader_.read_vec( (#count) as usize ).map( |_value_| _value_.into() ) }
    } else {
        quote! { _reader_.read_value() }
    };

    if default_on_eof {
        quote! {
            match #body {
                Ok( value ) => value,
                Err( ref error ) if error.kind() == std::io::ErrorKind::UnexpectedEof => std::default::Default::default(),
                Err( error ) => return Err( error )
            }
        }
    } else {
        quote! { #body? }
    }
}

fn readable_body< 'a >( types: &mut Vec< &'a syn::Type >, st: Struct< 'a > ) -> (TokenStream, TokenStream, TokenStream) {
    let mut field_names = Vec::new();
    let mut field_readers = Vec::new();
    let mut minimum_bytes_needed = Vec::new();
    for field in st.fields {
        types.push( field.ty );
        let name = field.var_name();

        let read_value = read_field_body( field.count.clone(), field.default_on_eof );
        field_readers.push( quote! { let #name = #read_value; } );
        field_names.push( name );

        if let Some( minimum_bytes ) = get_minimum_bytes( &field ) {
            minimum_bytes_needed.push( minimum_bytes );
        }
    }

    let body = quote! { #(#field_readers)* };
    let initializer = quote! { #(#field_names),* };
    let minimum_bytes_needed = sum( minimum_bytes_needed );
    (body, initializer, minimum_bytes_needed)
}

fn write_field_body( name: &syn::Ident, count: Option< syn::Expr > ) -> TokenStream {
    if let Some( count ) = count {
        let error_message = format!( "the length of '{}' is not the same as its 'count' attribute", name );
        quote! {
            let __expected = #count;
            if #name.len() != __expected as usize {
                return Err( std::io::Error::new( std::io::ErrorKind::InvalidData, #error_message ) );
            }
            _writer_.write_slice( &#name )?;
        }
    } else {
        quote! { _writer_.write_value( #name )?; }
    }
}

fn writable_body< 'a >( types: &mut Vec< &'a syn::Type >, st: Struct< 'a > ) -> (TokenStream, TokenStream) {
    let mut field_names = Vec::new();
    let mut field_writers = Vec::new();
    for field in st.fields {
        types.push( field.ty );

        let name = field.var_name();
        field_names.push( name.clone() );

        let write_value = write_field_body( &name, field.count.clone() );
        field_writers.push( write_value );
    }

    let body = quote! { #(#field_writers)* };
    let initializer = quote! { #(ref #field_names),* };
    (body, initializer)
}

struct EnumCtx {
    ident: syn::Ident,
    previous_kind: Option< u64 >,
    kind_to_full_name: HashMap< u64, String >,
    tag_type: BasicType
}

impl EnumCtx {
    fn new( ident: &syn::Ident, tag_type: Option< BasicType > ) -> Self {
        EnumCtx {
            ident: ident.clone(),
            previous_kind: None,
            kind_to_full_name: HashMap::new(),
            tag_type: tag_type.unwrap_or( BasicType::U32 )
        }
    }

    fn next( &mut self, variant: &syn::Variant ) -> Result< TokenStream, syn::Error > {
        let max = match self.tag_type {
            BasicType::U8 => std::u8::MAX as u64,
            BasicType::U16 => std::u16::MAX as u64,
            BasicType::U32 => std::u32::MAX as u64,
            BasicType::U64 => std::u64::MAX
        };

        let full_name = format!( "{}::{}", self.ident, variant.ident );
        let kind = match variant.discriminant {
            None => {
                let kind = if let Some( previous_kind ) = self.previous_kind {
                    if previous_kind >= max {
                        let message = format!( "Enum discriminant `{}` is too big!", full_name );
                        return Err( syn::Error::new( variant.span(), message ) );
                    }

                    previous_kind + 1
                } else {
                    0
                };

                self.previous_kind = Some( kind );
                kind
            },
            Some( (_, syn::Expr::Lit( syn::ExprLit { lit: syn::Lit::Int( ref raw_value ), .. } )) ) => {
                let value = raw_value.base10_parse::< u64 >().map_err( |err| {
                    syn::Error::new( raw_value.span(), err )
                })?;
                if value > max {
                    let message = format!( "Enum discriminant `{}` is too big!", full_name );
                    return Err( syn::Error::new( raw_value.span(), message ) );
                }

                self.previous_kind = Some( value );
                value
            },
            Some((_, ref expr)) => {
                let message = format!( "Enum discriminant `{}` is currently unsupported!", full_name );
                return Err( syn::Error::new( expr.span(), message ) );
            }
        };

        if let Some( other_full_name ) = self.kind_to_full_name.get( &kind ) {
            let message = format!( "Two discriminants with the same value of '{}': `{}`, `{}`", kind, full_name, other_full_name );
            return Err( syn::Error::new( variant.span(), message ) );
        }

        self.kind_to_full_name.insert( kind, full_name );
        let kind = match self.tag_type {
            BasicType::U8 => {
                let kind = kind as u8;
                quote! { #kind }
            },
            BasicType::U16 => {
                let kind = kind as u16;
                quote! { #kind }
            },
            BasicType::U32 => {
                let kind = kind as u32;
                quote! { #kind }
            },
            BasicType::U64 => {
                let kind = kind as u64;
                quote! { #kind }
            }
        };
        Ok( kind )
    }
}

fn get_minimum_bytes( field: &Field ) -> Option< TokenStream > {
    if field.default_on_eof || field.count.is_some() {
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

fn impl_readable( input: syn::DeriveInput ) -> Result< TokenStream, syn::Error > {
    let name = &input.ident;
    let mut types = Vec::new();
    let (reader_body, minimum_bytes_needed_body) = match &input.data {
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Named( syn::FieldsNamed { named, .. } ), .. } ) => {
            parse_item_attributes( ItemKind::Struct, &input.attrs )?;
            let st = Struct::new( named )?;
            let (body, initializer, minimum_bytes) = readable_body( &mut types, st );
            let reader_body = quote! {
                #body
                Ok( #name { #initializer } )
            };
            (reader_body, minimum_bytes)
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unnamed( syn::FieldsUnnamed { unnamed, .. } ), .. } ) => {
            parse_item_attributes( ItemKind::Struct, &input.attrs )?;
            let st = Struct::new( unnamed )?;
            let (body, initializer, minimum_bytes) = readable_body( &mut types, st );
            let reader_body = quote! {
                #body
                Ok( #name( #initializer ) )
            };
            (reader_body, minimum_bytes)
        },
        syn::Data::Enum( syn::DataEnum { variants, .. } ) => {
            let item_attrs = parse_item_attributes( ItemKind::Enum, &input.attrs )?;
            let mut ctx = EnumCtx::new( &name, item_attrs.tag_type );
            let mut variant_matches = Vec::with_capacity( variants.len() );
            let mut variant_minimum_sizes = Vec::with_capacity( variants.len() );
            for variant in variants {
                let kind = ctx.next( &variant )?;
                let unqualified_ident = &variant.ident;
                let variant_path = quote! { #name::#unqualified_ident };

                match variant.fields {
                    syn::Fields::Named( syn::FieldsNamed { ref named, .. } ) => {
                        let st = Struct::new( named )?;
                        let (body, initializer, minimum_bytes) = readable_body( &mut types, st );
                        variant_matches.push( quote! {
                            #kind => {
                                #body
                                Ok( #variant_path { #initializer } )
                            }
                        });
                        variant_minimum_sizes.push( minimum_bytes );
                    },
                    syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ) => {
                        let st = Struct::new( unnamed )?;
                        let (body, initializer, minimum_bytes) = readable_body( &mut types, st );
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
            }

            let (tag_reader, tag_size) = match ctx.tag_type {
                BasicType::U64 => (quote! { read_u64 }, 8_usize),
                BasicType::U32 => (quote! { read_u32 }, 4_usize),
                BasicType::U16 => (quote! { read_u16 }, 2_usize),
                BasicType::U8 => (quote! { read_u8 }, 1_usize)
            };

            let reader_body = quote! {
                let kind_ = _reader_.#tag_reader()?;
                match kind_ {
                    #(#variant_matches),*
                    _ => Err( std::io::Error::new( std::io::ErrorKind::InvalidData, "invalid enum variant" ) )
                }
            };
            let minimum_bytes_needed_body = min( variant_minimum_sizes.into_iter() );
            let minimum_bytes_needed_body = quote! { (#minimum_bytes_needed_body) + #tag_size };
            (reader_body, minimum_bytes_needed_body)
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unit, .. } ) => {
            parse_item_attributes( ItemKind::Struct, &input.attrs )?;
            let reader_body = quote! {
                Ok( #name )
            };

            let minimum_bytes_needed_body = quote! { 0 };
            (reader_body, minimum_bytes_needed_body)
        },
        syn::Data::Union( syn::DataUnion { union_token, .. } ) => {
            let message = "Unions are not supported!";
            return Err( syn::Error::new( union_token.span(), message ) );
        }
    };

    let (impl_params, ty_params, where_clause) = common_tokens( &input, &types, Variant::Readable );
    let output = quote! {
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
    };

    Ok( output )
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

fn impl_writable( input: syn::DeriveInput ) -> Result< TokenStream, syn::Error > {
    let name = &input.ident;
    let mut types = Vec::new();
    let writer_body = match input.data {
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unit, .. } ) => {
            parse_item_attributes( ItemKind::Struct, &input.attrs )?;
            quote! {}
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Named( syn::FieldsNamed { ref named, .. } ), .. } ) => {
            parse_item_attributes( ItemKind::Struct, &input.attrs )?;
            let st = Struct::new( named )?;
            let assignments = assign_to_variables( &st.fields );
            let (body, _) = writable_body( &mut types, st );
            quote! {
                #assignments
                #body
            }
        },
        syn::Data::Struct( syn::DataStruct { fields: syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ), .. } ) => {
            parse_item_attributes( ItemKind::Struct, &input.attrs )?;
            let st = Struct::new( unnamed )?;
            let assignments = assign_to_variables( &st.fields );
            let (body, _) = writable_body( &mut types, st );
            quote! {
                #assignments
                #body
            }
        },
        syn::Data::Enum( syn::DataEnum { ref variants, .. } ) => {
            let item_attrs = parse_item_attributes( ItemKind::Enum, &input.attrs )?;
            let mut ctx = EnumCtx::new( &name, item_attrs.tag_type );
            let tag_writer = match ctx.tag_type {
                BasicType::U64 => quote! { write_u64 },
                BasicType::U32 => quote! { write_u32 },
                BasicType::U16 => quote! { write_u16 },
                BasicType::U8 => quote! { write_u8 }
            };

            let variants: Result< Vec< _ >, syn::Error > = variants.iter()
                .map( |variant| {
                    let kind = ctx.next( &variant )?;
                    let unqualified_ident = &variant.ident;
                    let variant_path = quote! { #name::#unqualified_ident };
                    let snippet = match variant.fields {
                        syn::Fields::Named( syn::FieldsNamed { ref named, .. } ) => {
                            let st = Struct::new( named )?;
                            let (body, identifiers) = writable_body( &mut types, st );
                            quote! {
                                #variant_path { #identifiers } => {
                                    _writer_.#tag_writer( #kind )?;
                                    #body
                                }
                            }
                        },
                        syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ) => {
                            let st = Struct::new( unnamed )?;
                            let (body, identifiers) = writable_body( &mut types, st );
                            quote! {
                                #variant_path( #identifiers ) => {
                                    _writer_.#tag_writer( #kind )?;
                                    #body
                                }
                            }
                        },
                        syn::Fields::Unit => {
                            quote! { #variant_path => {
                                _writer_.#tag_writer( #kind )?;
                            }}
                        },
                    };

                    Ok( snippet )
                })
                .collect();
            let variants = variants?;
            quote! { match *self { #(#variants),* } }
        },
        syn::Data::Union( syn::DataUnion { union_token, .. } ) => {
            let message = "Unions are not supported!";
            return Err( syn::Error::new( union_token.span(), message ) );
        }
    };

    let (impl_params, ty_params, where_clause) = common_tokens( &input, &types, Variant::Writable );
    let output = quote! {
        impl< #impl_params C_: speedy::Context > speedy::Writable< C_ > for #name #ty_params #where_clause {
            #[inline]
            fn write_to< T_: ?Sized + speedy::Writer< C_ > >( &self, _writer_: &mut T_ ) -> std::io::Result< () > {
                #writer_body
                Ok(())
            }
        }
    };

    Ok( output )
}
