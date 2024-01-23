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
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::TypeParam;

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
    syn::custom_keyword!( length );
    syn::custom_keyword!( default_on_eof );
    syn::custom_keyword!( tag_type );
    syn::custom_keyword!( length_type );
    syn::custom_keyword!( tag );
    syn::custom_keyword!( skip );
    syn::custom_keyword!( constant_prefix );
    syn::custom_keyword!( peek_tag );
    syn::custom_keyword!( varint );
    syn::custom_keyword!( unsafe_is_primitive );
    syn::custom_keyword!( always );

    syn::custom_keyword!( u7 );
    syn::custom_keyword!( u8 );
    syn::custom_keyword!( u16 );
    syn::custom_keyword!( u32 );
    syn::custom_keyword!( u64 );
    syn::custom_keyword!( u64_varint );
}

#[derive(Copy, Clone, PartialEq)]
enum Trait {
    Readable,
    Writable,
    ZeroCopyable { is_packed: bool, is_forced: bool }
}

fn uses_generics( input: &syn::DeriveInput ) -> bool {
    input.generics.type_params().next().is_some() ||
    input.generics.lifetimes().next().is_some() ||
    input.generics.const_params().next().is_some()
}

fn possibly_uses_generic_ty( generic_types: &[&syn::Ident], ty: &syn::Type ) -> bool {
    match ty {
        syn::Type::Path( syn::TypePath { qself: None, path: syn::Path { leading_colon: None, segments } } ) => {
            segments.iter().any( |segment| {
                if generic_types.iter().any( |&ident| *ident == segment.ident ) {
                    return true;
                }

                match segment.arguments {
                    syn::PathArguments::None => false,
                    syn::PathArguments::AngleBracketed( syn::AngleBracketedGenericArguments { ref args, .. } ) => {
                        args.iter().any( |arg| {
                            match arg {
                                syn::GenericArgument::Lifetime( .. ) => false,
                                syn::GenericArgument::Type( inner_ty ) => possibly_uses_generic_ty( generic_types, inner_ty ),
                                syn::GenericArgument::AssocType( assoc_type ) => possibly_uses_generic_ty( generic_types, &assoc_type.ty ),
                                // TODO: How to handle these?
                                syn::GenericArgument::Constraint( .. ) => true,
                                syn::GenericArgument::Const( .. ) => true,
                                syn::GenericArgument::AssocConst(_) => true,
                                _ => true,
                            }
                        })
                    },
                    _ => true
                }
            })
        },
        syn::Type::Slice( syn::TypeSlice { elem, .. } ) => possibly_uses_generic_ty( generic_types, elem ),
        syn::Type::Tuple( syn::TypeTuple { elems, .. } ) => elems.iter().any( |elem| possibly_uses_generic_ty( generic_types, elem ) ),
        syn::Type::Reference( syn::TypeReference { elem, .. } ) => possibly_uses_generic_ty( generic_types, elem ),
        syn::Type::Paren( syn::TypeParen { elem, .. } ) => possibly_uses_generic_ty( generic_types, elem ),
        syn::Type::Ptr( syn::TypePtr { elem, .. } ) => possibly_uses_generic_ty( generic_types, elem ),
        syn::Type::Group( syn::TypeGroup { elem, .. } ) => possibly_uses_generic_ty( generic_types, elem ),
        syn::Type::Array( syn::TypeArray { elem, len, .. } ) => {
            if possibly_uses_generic_ty( generic_types, elem ) {
                return true;
            }

            // This is probably too conservative.
            match len {
                syn::Expr::Lit( .. ) => false,
                _ => true
            }
        },
        syn::Type::Never( .. ) => false,
        _ => true
    }
}

#[test]
fn test_possibly_uses_generic_ty() {
    macro_rules! assert_test {
        ($result:expr, $($token:tt)+) => {
            assert_eq!(
                possibly_uses_generic_ty( &[&syn::Ident::new( "T", proc_macro2::Span::call_site() )], &syn::parse2( quote! { $($token)+ } ).unwrap() ),
                $result
            );
        }
    }

    assert_test!( false, String );
    assert_test!( false, Cow<'a, BTreeMap<u8, u8>> );
    assert_test!( false, Cow<'a, [u8]> );
    assert_test!( false, () );
    assert_test!( false, (u8) );
    assert_test!( false, (u8, u8) );
    assert_test!( false, &u8 );
    assert_test!( false, *const u8 );
    assert_test!( false, ! );
    assert_test!( false, [u8; 2] );
    assert_test!( true, T );
    assert_test!( true, Dummy::T );
    assert_test!( true, Cow<'a, BTreeMap<T, u8>> );
    assert_test!( true, Cow<'a, BTreeMap<u8, T>> );
    assert_test!( true, Cow<'a, [T]> );
    assert_test!( true, (T) );
    assert_test!( true, (u8, T) );
    assert_test!( true, &T );
    assert_test!( true, *const T );
    assert_test!( true, [T; 2] );
    assert_test!( true, Vec<T> );
}

fn is_guaranteed_non_recursive( ty: &syn::Type ) -> bool {
    match ty {
        syn::Type::Path( syn::TypePath { qself: None, path: syn::Path { leading_colon: None, segments } } ) => {
            if segments.len() != 1 {
                return false;
            }

            let segment = &segments[ 0 ];
            let ident = segment.ident.to_string();
            match ident.as_str() {
                "String" | "Vec" | "BTreeSet" | "BTreeMap" | "HashSet" | "HashMap" |
                "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "usize" | "isize" |
                "str" => {},
                _ => return false
            }

            match segment.arguments {
                syn::PathArguments::None => true,
                syn::PathArguments::AngleBracketed( syn::AngleBracketedGenericArguments { ref args, .. } ) => {
                    args.iter().all( |arg| {
                            match arg {
                                syn::GenericArgument::Lifetime( .. ) => true,
                                syn::GenericArgument::Type( inner_ty ) => is_guaranteed_non_recursive( inner_ty ),
                                syn::GenericArgument::AssocType( assoc_type ) => is_guaranteed_non_recursive( &assoc_type.ty ),
                                // TODO: How to handle these?
                                syn::GenericArgument::Constraint( .. ) => false,
                                syn::GenericArgument::Const( .. ) => false,
                                syn::GenericArgument::AssocConst(_) => false,
                                _ => false,
                            }
                        })
                },
                _ => false
            }
        },
        syn::Type::Slice( syn::TypeSlice { elem, .. } ) => is_guaranteed_non_recursive( elem ),
        syn::Type::Tuple( syn::TypeTuple { elems, .. } ) => elems.iter().all(is_guaranteed_non_recursive),
        syn::Type::Reference( syn::TypeReference { elem, .. } ) => is_guaranteed_non_recursive( elem ),
        syn::Type::Paren( syn::TypeParen { elem, .. } ) => is_guaranteed_non_recursive( elem ),
        syn::Type::Ptr( syn::TypePtr { elem, .. } ) => is_guaranteed_non_recursive( elem ),
        syn::Type::Group( syn::TypeGroup { elem, .. } ) => is_guaranteed_non_recursive( elem ),
        syn::Type::Array( syn::TypeArray { elem, len, .. } ) => {
            if !is_guaranteed_non_recursive( elem ) {
                return false;
            }

            // This is probably too conservative.
            match len {
                syn::Expr::Lit( .. ) => true,
                _ => false
            }
        },
        syn::Type::Never( .. ) => true,
        _ => false
    }
}

#[test]
fn test_is_guaranteed_non_recursive() {
    macro_rules! assert_test {
        ($result:expr, $($token:tt)+) => {
            assert_eq!(
                is_guaranteed_non_recursive( &syn::parse2( quote! { $($token)+ } ).unwrap() ),
                $result
            );
        }
    }

    assert_test!( true, String );
    assert_test!( true, u8 );
    assert_test!( true, () );
    assert_test!( true, *const u8 );
    assert_test!( true, [u8; 2] );
    assert_test!( true, (u8, u16) );
    assert_test!( true, ! );
    assert_test!( true, Vec< u8 > );
    assert_test!( false, T );
    assert_test!( false, Vec< T > );
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum PrimitiveTy {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64
}

fn parse_primitive_ty( ty: &syn::Type ) -> Option< PrimitiveTy > {
    match ty {
        syn::Type::Path( syn::TypePath { qself: None, path: syn::Path { leading_colon: None, segments } } ) => {
            if segments.len() != 1 {
                return None;
            }

            let segment = &segments[ 0 ];
            let ident = segment.ident.to_string();
            match ident.as_str() {
                "u8" => Some( PrimitiveTy::U8 ),
                "u16" => Some( PrimitiveTy::U16 ),
                "u32" => Some( PrimitiveTy::U32 ),
                "u64" => Some( PrimitiveTy::U64 ),
                "u128" => Some( PrimitiveTy::U128 ),
                "i8" => Some( PrimitiveTy::I8 ),
                "i16" => Some( PrimitiveTy::I16 ),
                "i32" => Some( PrimitiveTy::I32 ),
                "i64" => Some( PrimitiveTy::I64 ),
                "i128" => Some( PrimitiveTy::I128 ),
                "f32" => Some( PrimitiveTy::F32 ),
                "f64" => Some( PrimitiveTy::F64 ),
                _ => None
            }
        },
        _ => None
    }
}

fn common_tokens( ast: &syn::DeriveInput, types: &[syn::Type], trait_variant: Trait ) -> (TokenStream, TokenStream, TokenStream) {
    let impl_params = {
        let lifetime_params = ast.generics.lifetimes().map( |alpha| quote! { #alpha } );
        let type_params = ast.generics.type_params().map( |ty| { let ty_without_default = TypeParam { default: None, ..ty.clone() }; quote! { #ty_without_default } });
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
        let constraints = types.iter().filter_map( |ty| {
            let possibly_generic = possibly_uses_generic_ty( &generics, ty );
            match (trait_variant, possibly_generic) {
                (Trait::Readable, true) => Some( quote! { #ty: speedy::Readable< 'a_, C_ > } ),
                (Trait::Readable, false) => None,
                (Trait::Writable, true) => Some( quote! { #ty: speedy::Writable< C_ > } ),
                (Trait::Writable, false) => None,
                (Trait::ZeroCopyable { is_packed, is_forced }, _) => {
                    if is_forced || (is_packed && parse_primitive_ty( ty ).is_some()) {
                        None
                    } else {
                        Some( quote! { #ty: speedy::private::ZeroCopyable< C_, T_ > } )
                    }
                },
            }
        });

        let mut predicates = Vec::new();
        if let Some( where_clause ) = ast.generics.where_clause.as_ref() {
            predicates = where_clause.predicates.iter().map( |pred| quote! { #pred } ).collect();
        }

        if trait_variant == Trait::Readable {
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

#[derive(Copy, Clone)]
enum BasicType {
    U7,
    U8,
    U16,
    U32,
    U64,
    VarInt64
}

const DEFAULT_LENGTH_TYPE: BasicType = BasicType::U32;
const DEFAULT_ENUM_TAG_TYPE: BasicType = BasicType::U32;

impl syn::parse::Parse for BasicType {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        let ty = if lookahead.peek( kw::u7 ) {
            input.parse::< kw::u7 >()?;
            BasicType::U7
        } else if lookahead.peek( kw::u8 ) {
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
        } else if lookahead.peek( kw::u64_varint ) {
            input.parse::< kw::u64_varint >()?;
            BasicType::VarInt64
        } else {
            return Err( lookahead.error() );
        };

        Ok( ty )
    }
}

enum IsPrimitive {
    Always,
}

impl syn::parse::Parse for IsPrimitive {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        let ty = if lookahead.peek( kw::always ) {
            input.parse::< kw::always >()?;
            IsPrimitive::Always
        } else {
            return Err( lookahead.error() );
        };

        Ok( ty )
    }
}

enum ToplevelStructAttribute {
    UnsafeIsPrimitive { key_token: kw::unsafe_is_primitive, kind: IsPrimitive },
    StructAttribute( StructAttribute )
}

enum VariantAttribute {
    Tag {
        key_token: kw::tag,
        tag: u64
    }
}

enum StructAttribute {
}

enum EnumAttribute {
    TagType {
        key_token: kw::tag_type,
        ty: BasicType
    },
    PeekTag {
        key_token: kw::peek_tag
    }
}

enum VariantOrStructAttribute {
    Variant( VariantAttribute ),
    Struct( StructAttribute )
}

fn parse_variant_attribute(
    input: &syn::parse::ParseStream,
    lookahead: &syn::parse::Lookahead1
) -> syn::parse::Result< Option< VariantAttribute > >
{
    let attribute = if lookahead.peek( kw::tag ) {
        let key_token = input.parse::< kw::tag >()?;
        let _: Token![=] = input.parse()?;
        let raw_tag: syn::LitInt = input.parse()?;
        let tag = raw_tag.base10_parse::< u64 >().map_err( |err| {
            syn::Error::new( raw_tag.span(), err )
        })?;

        VariantAttribute::Tag {
            key_token,
            tag
        }
    } else {
        return Ok( None )
    };

    Ok( Some( attribute ) )
}

fn parse_struct_attribute(
    _input: &syn::parse::ParseStream,
    _lookahead: &syn::parse::Lookahead1
) -> syn::parse::Result< Option< StructAttribute > >
{
    Ok( None )
}

fn parse_enum_attribute(
    input: &syn::parse::ParseStream,
    lookahead: &syn::parse::Lookahead1
) -> syn::parse::Result< Option< EnumAttribute > >
{
    let attribute = if lookahead.peek( kw::tag_type ) {
        let key_token = input.parse::< kw::tag_type >()?;
        let _: Token![=] = input.parse()?;
        let ty: BasicType = input.parse()?;

        EnumAttribute::TagType {
            key_token,
            ty
        }
    } else if lookahead.peek( kw::peek_tag ) {
        let key_token = input.parse::< kw::peek_tag >()?;
        EnumAttribute::PeekTag {
            key_token
        }
    } else {
        return Ok( None )
    };

    Ok( Some( attribute ) )
}

impl syn::parse::Parse for ToplevelStructAttribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        if lookahead.peek( kw::unsafe_is_primitive ) {
            let key_token = input.parse::< kw::unsafe_is_primitive >()?;
            let _: Token![=] = input.parse()?;
            let kind = input.parse::< IsPrimitive >()?;
            Ok( ToplevelStructAttribute::UnsafeIsPrimitive { key_token, kind } )
        } else if let Some( attr ) = parse_struct_attribute( &input, &lookahead )? {
            Ok( ToplevelStructAttribute::StructAttribute( attr ) )
        } else {
            Err( lookahead.error() )
        }
    }
}

impl syn::parse::Parse for StructAttribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        parse_struct_attribute( &input, &lookahead )?.ok_or_else( || lookahead.error() )
    }
}

impl syn::parse::Parse for EnumAttribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        parse_enum_attribute( &input, &lookahead )?.ok_or_else( || lookahead.error() )
    }
}

impl syn::parse::Parse for VariantOrStructAttribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        if let Some( attr ) = parse_variant_attribute( &input, &lookahead )? {
            return Ok( VariantOrStructAttribute::Variant( attr ) );
        }

        if let Some( attr ) = parse_struct_attribute( &input, &lookahead )? {
            return Ok( VariantOrStructAttribute::Struct( attr ) );
        }

        Err( lookahead.error() )
    }
}

struct VariantAttributes {
    tag: Option< u64 >
}

struct StructAttributes {
}

struct EnumAttributes {
    tag_type: Option< BasicType >,
    peek_tag: bool
}

struct ToplevelStructAttributes {
    is_primitive: Option< IsPrimitive >,
    struct_attributes: StructAttributes
}

impl std::ops::Deref for ToplevelStructAttributes {
    type Target = StructAttributes;
    fn deref( &self ) -> &Self::Target {
        &self.struct_attributes
    }
}

fn check_repr( attrs: &[syn::Attribute], value: &str ) -> bool {
    let mut result = false;
    for raw_attr in attrs {
        let path = raw_attr.meta.path().clone().into_token_stream().to_string();
        if path != "repr" {
            continue;
        }

        if let Ok(meta_list) = raw_attr.meta.require_list() {
            result = meta_list.tokens.clone().into_token_stream().to_string() == value;
        }
    }

    result
}

fn is_transparent( attrs: &[syn::Attribute] ) -> bool {
    check_repr( attrs, "transparent" )
}

fn is_packed( attrs: &[syn::Attribute] ) -> bool {
    check_repr( attrs, "packed" )
}

fn is_c( attrs: &[syn::Attribute] ) -> bool {
    check_repr( attrs, "C" )
}

fn parse_attributes< T >( attrs: &[syn::Attribute] ) -> Result< Vec< T >, syn::Error > where T: syn::parse::Parse {
    struct RawAttributes< T >( Punctuated< T, Token![,] > );

    impl< T > syn::parse::Parse for RawAttributes< T > where T: syn::parse::Parse {
        fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
            let content;
            parenthesized!( content in input );
            Ok( RawAttributes( content.parse_terminated( T::parse, Token![,] )? ) )
        }
    }

    let mut output = Vec::new();
    for raw_attr in attrs {
        let path = raw_attr.meta.path().clone().into_token_stream().to_string();
        if path != "speedy" {
            continue;
        }

        let meta_list = raw_attr.meta.require_list()?;
        let parsed_attrs = syn::parse::Parser::parse2(
            Punctuated::<T, Token![,]>::parse_terminated,
            meta_list.tokens.clone(),
        )?;
        for attr in parsed_attrs {
            output.push( attr );
        }
    }

    Ok( output )
}

fn collect_variant_attributes( attrs: Vec< VariantAttribute > ) -> Result< VariantAttributes, syn::Error > {
    let mut variant_tag = None;
    for attr in attrs {
        match attr {
            VariantAttribute::Tag { key_token, tag } => {
                if variant_tag.is_some() {
                    let message = "Duplicate 'tag'";
                    return Err( syn::Error::new( key_token.span(), message ) );
                }
                variant_tag = Some( tag );
            }
        }
    }

    Ok( VariantAttributes {
        tag: variant_tag
    })
}

fn collect_struct_attributes( attrs: Vec< StructAttribute > ) -> Result< StructAttributes, syn::Error > {
    for _attr in attrs {
    }

    Ok( StructAttributes {
    })
}

fn collect_toplevel_struct_attributes( attrs: Vec< ToplevelStructAttribute > ) -> Result< ToplevelStructAttributes, syn::Error > {
    let mut struct_attributes = Vec::new();
    let mut is_primitive = None;
    for attr in attrs {
        match attr {
            ToplevelStructAttribute::UnsafeIsPrimitive { key_token, kind } => {
                if is_primitive.is_some() {
                    let message = "Duplicate 'unsafe_is_primitive'";
                    return Err( syn::Error::new( key_token.span(), message ) );
                }
                is_primitive = Some( kind );
            },
            ToplevelStructAttribute::StructAttribute( attr ) => {
                struct_attributes.push( attr );
            }
        }
    }

    let struct_attributes = collect_struct_attributes( struct_attributes )?;
    Ok( ToplevelStructAttributes {
        is_primitive,
        struct_attributes
    })
}


fn collect_enum_attributes( attrs: Vec< EnumAttribute > ) -> Result< EnumAttributes, syn::Error > {
    let mut tag_type = None;
    let mut peek_tag = false;
    for attr in attrs {
        match attr {
            EnumAttribute::TagType { key_token, ty } => {
                if tag_type.is_some() {
                    let message = "Duplicate 'tag_type'";
                    return Err( syn::Error::new( key_token.span(), message ) );
                }
                tag_type = Some( ty );
            },
            EnumAttribute::PeekTag { key_token } => {
                if peek_tag {
                    let message = "Duplicate 'peek_tag'";
                    return Err( syn::Error::new( key_token.span(), message ) );
                }
                peek_tag = true;
            }
        }
    }

    Ok( EnumAttributes {
        tag_type,
        peek_tag
    })
}

#[derive(PartialEq)]
enum StructKind {
    Unit,
    Named,
    Unnamed
}

struct Struct< 'a > {
    fields: Vec< Field< 'a > >,
    kind: StructKind
}

impl< 'a > Struct< 'a > {
    fn new( fields: &'a syn::Fields, _attrs: &StructAttributes ) -> Result< Self, syn::Error > {
        let structure = match fields {
            syn::Fields::Unit => {
                Struct {
                    fields: Vec::new(),
                    kind: StructKind::Unit
                }
            },
            syn::Fields::Named( syn::FieldsNamed { ref named, .. } ) => {
                Struct {
                    fields: get_fields( named.into_iter() )?,
                    kind: StructKind::Named
                }
            },
            syn::Fields::Unnamed( syn::FieldsUnnamed { ref unnamed, .. } ) => {
                Struct {
                    fields: get_fields( unnamed.into_iter() )?,
                    kind: StructKind::Unnamed
                }
            }
        };

        Ok( structure )
    }

    fn is_guaranteed_non_recursive( &self ) -> bool {
        self.fields.iter().all( |field| field.is_guaranteed_non_recursive() )
    }
}

struct Field< 'a > {
    index: usize,
    name: Option< &'a syn::Ident >,
    raw_ty: &'a syn::Type,
    default_on_eof: bool,
    length: Option< LengthKind >,
    length_type: Option< BasicType >,
    ty: Opt< Ty >,
    skip: bool,
    varint: bool,
    constant_prefix: Option< syn::LitByteStr >
}

impl< 'a > Field< 'a > {
    fn can_be_primitive( &self ) -> bool {
        !self.default_on_eof &&
        self.length.is_none() &&
        self.length_type.is_none() &&
        !self.skip &&
        !self.varint &&
        self.constant_prefix.is_none()
    }

    fn is_simple( &self ) -> bool {
        parse_primitive_ty( self.raw_ty ).is_some()
    }

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

    fn bound_types( &self ) -> Vec< syn::Type > {
        match self.ty.inner() {
            | Ty::Array( inner_ty, .. )
            | Ty::Vec( inner_ty )
            | Ty::HashSet( inner_ty )
            | Ty::BTreeSet( inner_ty )
            | Ty::CowHashSet( _, inner_ty )
            | Ty::CowBTreeSet( _, inner_ty )
            | Ty::CowSlice( _, inner_ty )
            | Ty::RefSlice( _, inner_ty )
                => vec![ inner_ty.clone() ],
            | Ty::HashMap( key_ty, value_ty )
            | Ty::BTreeMap( key_ty, value_ty )
            | Ty::CowHashMap( _, key_ty, value_ty )
            | Ty::CowBTreeMap( _, key_ty, value_ty )
                => vec![ key_ty.clone(), value_ty.clone() ],
            | Ty::RefSliceU8( _ )
            | Ty::RefStr( _ )
            | Ty::String
            | Ty::CowStr( .. )
            | Ty::Primitive( .. )
                => vec![],
            | Ty::Ty( _ ) => vec![ self.raw_ty.clone() ]
        }
    }

    fn is_guaranteed_non_recursive( &self ) -> bool {
        is_guaranteed_non_recursive( self.raw_ty )
    }
}

enum LengthKind {
    Expr( syn::Expr ),
    UntilEndOfFile
}

impl syn::parse::Parse for LengthKind {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        if input.peek( Token![..] ) {
            let _: Token![..] = input.parse()?;
            Ok( LengthKind::UntilEndOfFile )
        } else {
            let expr: syn::Expr = input.parse()?;
            Ok( LengthKind::Expr( expr ) )
        }
    }
}

enum FieldAttribute {
    DefaultOnEof {
        key_span: Span
    },
    Length {
        key_span: Span,
        length_kind: LengthKind
    },
    LengthType {
        key_span: Span,
        ty: BasicType
    },
    Skip {
        key_span: Span
    },
    ConstantPrefix {
        key_span: Span,
        prefix: syn::LitByteStr
    },
    VarInt {
        key_span: Span
    }
}

impl syn::parse::Parse for FieldAttribute {
    fn parse( input: syn::parse::ParseStream ) -> syn::parse::Result< Self > {
        let lookahead = input.lookahead1();
        let value = if lookahead.peek( kw::default_on_eof ) {
            let key_token = input.parse::< kw::default_on_eof >()?;
            FieldAttribute::DefaultOnEof {
                key_span: key_token.span()
            }
        } else if lookahead.peek( kw::length ) {
            let key_token = input.parse::< kw::length >()?;
            let _: Token![=] = input.parse()?;
            let length_kind: LengthKind = input.parse()?;
            FieldAttribute::Length {
                key_span: key_token.span(),
                length_kind
            }
        } else if lookahead.peek( kw::length_type ) {
            let key_token = input.parse::< kw::length_type>()?;
            let _: Token![=] = input.parse()?;
            let ty: BasicType = input.parse()?;
            FieldAttribute::LengthType {
                key_span: key_token.span(),
                ty
            }
        } else if lookahead.peek( kw::skip ) {
            let key_token = input.parse::< kw::skip >()?;
            FieldAttribute::Skip {
                key_span: key_token.span()
            }
        } else if lookahead.peek( kw::constant_prefix ) {
            let key_token = input.parse::< kw::constant_prefix >()?;
            let _: Token![=] = input.parse()?;
            let expr: syn::Expr = input.parse()?;
            let value_span = expr.span();
            let generic_error = || {
                Err( syn::Error::new( value_span, "unsupported expression; only basic literals are supported" ) )
            };

            let prefix = match expr {
                syn::Expr::Lit( literal ) => {
                    let literal = literal.lit;
                    match literal {
                        syn::Lit::Str( literal ) => literal.value().into_bytes(),
                        syn::Lit::ByteStr( literal ) => literal.value(),
                        syn::Lit::Byte( literal ) => vec![ literal.value() ],
                        syn::Lit::Char( literal ) => format!( "{}", literal.value() ).into_bytes(),
                        syn::Lit::Bool( literal ) => vec![ if literal.value { 1 } else { 0 } ],
                        syn::Lit::Int( literal ) => {
                            if literal.suffix() == "u8" {
                                vec![ literal.base10_parse::< u8 >().unwrap() ]
                            } else {
                                return Err( syn::Error::new( value_span, "integers are not supported; if you want to use a single byte constant then append either 'u8' or 'i8' to it" ) );
                            }
                        },
                        syn::Lit::Float( _ ) => return Err( syn::Error::new( value_span, "floats are not supported" ) ),
                        syn::Lit::Verbatim( _ ) => return Err( syn::Error::new( value_span, "verbatim literals are not supported" ) ),
                        _ => return Err( syn::Error::new( value_span, "unsupported literal" ) )
                    }
                },
                syn::Expr::Unary( syn::ExprUnary { op: syn::UnOp::Neg(_), expr, .. } ) => {
                    match *expr {
                        syn::Expr::Lit( syn::ExprLit { lit: syn::Lit::Int( literal ), .. } ) => {
                            if literal.suffix() == "i8" {
                                vec![ -literal.base10_parse::< i8 >().unwrap() as u8 ]
                            } else if literal.suffix() == "u8" {
                                return generic_error()
                            } else {
                                return Err( syn::Error::new( value_span, "integers are not supported; if you want to use a single byte constant then append either 'u8' or 'i8' to it" ) );
                            }
                        },
                        _ => return generic_error()
                    }
                },
                _ => return generic_error()
            };

            FieldAttribute::ConstantPrefix {
                key_span: key_token.span(),
                prefix: syn::LitByteStr::new( &prefix, value_span )
            }
        } else if lookahead.peek( kw::varint ) {
            let key_token = input.parse::< kw::varint >()?;
            FieldAttribute::VarInt {
                key_span: key_token.span()
            }
        } else {
            return Err( lookahead.error() )
        };

        Ok( value )
    }
}

enum Opt< T > {
    Plain( T ),
    Option( T )
}

impl< T > Opt< T > {
    fn inner( &self ) -> &T {
        match *self {
            Opt::Option( ref inner ) => inner,
            Opt::Plain( ref inner ) => inner
        }
    }
}

enum Ty {
    String,
    Vec( syn::Type ),
    CowSlice( syn::Lifetime, syn::Type ),
    CowStr( syn::Lifetime ),
    HashMap( syn::Type, syn::Type ),
    HashSet( syn::Type ),
    BTreeMap( syn::Type, syn::Type ),
    BTreeSet( syn::Type ),

    CowHashMap( syn::Lifetime, syn::Type, syn::Type ),
    CowHashSet( syn::Lifetime, syn::Type ),
    CowBTreeMap( syn::Lifetime, syn::Type, syn::Type ),
    CowBTreeSet( syn::Lifetime, syn::Type ),

    RefSliceU8( syn::Lifetime ),
    RefSlice( syn::Lifetime, syn::Type ),
    RefStr( syn::Lifetime ),

    Array( syn::Type, u32 ),

    Primitive( PrimitiveTy ),
    Ty( syn::Type )
}

fn extract_inner_ty( args: &Punctuated< syn::GenericArgument, syn::token::Comma > ) -> Option< &syn::Type > {
    if args.len() != 1 {
        return None;
    }

    match args[ 0 ] {
        syn::GenericArgument::Type( ref ty ) => Some( ty ),
         _ => None
    }
}

fn extract_inner_ty_2( args: &Punctuated< syn::GenericArgument, syn::token::Comma > ) -> Option< (&syn::Type, &syn::Type) > {
    if args.len() != 2 {
        return None;
    }

    let ty_1 = match args[ 0 ] {
        syn::GenericArgument::Type( ref ty ) => ty,
         _ => return None
    };

    let ty_2 = match args[ 1 ] {
        syn::GenericArgument::Type( ref ty ) => ty,
         _ => return None
    };

    Some( (ty_1, ty_2) )
}

fn extract_lifetime_and_inner_ty( args: &Punctuated< syn::GenericArgument, syn::token::Comma > ) -> Option< (&syn::Lifetime, &syn::Type) > {
    if args.len() != 2 {
        return None;
    }

    let lifetime = match args[ 0 ] {
        syn::GenericArgument::Lifetime( ref lifetime ) => lifetime,
        _ => return None
    };

    let ty = match args[ 1 ] {
        syn::GenericArgument::Type( ref ty ) => ty,
         _ => return None
    };

    Some( (lifetime, ty) )
}

fn extract_slice_inner_ty( ty: &syn::Type ) -> Option< &syn::Type > {
    match *ty {
        syn::Type::Slice( syn::TypeSlice { ref elem, .. } ) => {
            Some( &*elem )
        },
        _ => None
    }
}

fn is_bare_ty( ty: &syn::Type, name: &str ) -> bool {
    match *ty {
        syn::Type::Path( syn::TypePath { path: syn::Path { leading_colon: None, ref segments }, qself: None } ) if segments.len() == 1 => {
            segments[ 0 ].ident == name && segments[ 0 ].arguments.is_empty()
        },
        _ => false
    }
}

fn extract_option_inner_ty( ty: &syn::Type ) -> Option< &syn::Type > {
    match *ty {
        syn::Type::Path( syn::TypePath { path: syn::Path { leading_colon: None, ref segments }, qself: None } )
            if segments.len() == 1 && segments[ 0 ].ident == "Option" =>
        {
            match segments[ 0 ].arguments {
                syn::PathArguments::AngleBracketed( syn::AngleBracketedGenericArguments { colon2_token: None, ref args, .. } ) if args.len() == 1 => {
                    match args[ 0 ] {
                        syn::GenericArgument::Type( ref ty ) => Some( ty ),
                        _ => None
                    }
                },
                _ => None
            }
        },
        _ => None
    }
}

fn parse_ty( ty: &syn::Type ) -> Ty {
    parse_special_ty( ty ).unwrap_or_else( || Ty::Ty( ty.clone() ) )
}

fn parse_special_ty( ty: &syn::Type ) -> Option< Ty > {
    if let Some( ty ) = parse_primitive_ty( ty ) {
        return Some( Ty::Primitive( ty ) );
    }

    match *ty {
        syn::Type::Path( syn::TypePath { path: syn::Path { leading_colon: None, ref segments }, qself: None } ) if segments.len() == 1 => {
            let name = &segments[ 0 ].ident;
            match segments[ 0 ].arguments {
                syn::PathArguments::None => {
                    if name == "String" {
                        Some( Ty::String )
                    } else {
                        None
                    }
                },
                syn::PathArguments::AngleBracketed( syn::AngleBracketedGenericArguments { colon2_token: None, ref args, .. } ) => {
                    if name == "Vec" {
                        Some( Ty::Vec( extract_inner_ty( args )?.clone() ) )
                    } else if name == "HashSet" {
                        Some( Ty::HashSet( extract_inner_ty( args )?.clone() ) )
                    } else if name == "BTreeSet" {
                        Some( Ty::BTreeSet( extract_inner_ty( args )?.clone() ) )
                    } else if name == "Cow" {
                        let (lifetime, ty) = extract_lifetime_and_inner_ty( args )?;
                        if let Some( inner_ty ) = extract_slice_inner_ty( ty ) {
                            Some( Ty::CowSlice( lifetime.clone(), inner_ty.clone() ) )
                        } else if is_bare_ty( ty, "str" ) {
                            Some( Ty::CowStr( lifetime.clone() ) )
                        } else {
                            match *ty {
                                syn::Type::Path( syn::TypePath { path: syn::Path { leading_colon: None, ref segments }, qself: None } ) if segments.len() == 1 => {
                                    let inner_name = &segments[ 0 ].ident;
                                    match segments[ 0 ].arguments {
                                        syn::PathArguments::AngleBracketed( syn::AngleBracketedGenericArguments { colon2_token: None, ref args, .. } ) => {
                                            if inner_name == "HashSet" {
                                                Some( Ty::CowHashSet( lifetime.clone(), extract_inner_ty( args )?.clone() ) )
                                            } else if inner_name == "BTreeSet" {
                                                Some( Ty::CowBTreeSet( lifetime.clone(), extract_inner_ty( args )?.clone() ) )
                                            } else if inner_name == "HashMap" {
                                                let (key_ty, value_ty) = extract_inner_ty_2( args )?;
                                                Some( Ty::CowHashMap( lifetime.clone(), key_ty.clone(), value_ty.clone() ) )
                                            } else if inner_name == "BTreeMap" {
                                                let (key_ty, value_ty) = extract_inner_ty_2( args )?;
                                                Some( Ty::CowBTreeMap( lifetime.clone(), key_ty.clone(), value_ty.clone() ) )
                                            } else {
                                                None
                                            }
                                        },
                                        _ => None
                                    }
                                },
                                _ => None
                            }
                        }
                    } else if name == "HashMap" {
                        let (key_ty, value_ty) = extract_inner_ty_2( args )?;
                        Some( Ty::HashMap( key_ty.clone(), value_ty.clone() ) )
                    } else if name == "BTreeMap" {
                        let (key_ty, value_ty) = extract_inner_ty_2( args )?;
                        Some( Ty::BTreeMap( key_ty.clone(), value_ty.clone() ) )
                    } else {
                        None
                    }
                },
                _ => None
            }
        },
        syn::Type::Array( syn::TypeArray {
            ref elem,
            len: syn::Expr::Lit( syn::ExprLit {
                ref attrs,
                lit: syn::Lit::Int( ref literal )
            }),
            ..
        }) if attrs.is_empty() => {
            if let Ok( length ) = literal.base10_parse::< u32 >() {
                Some( Ty::Array( (**elem).clone(), length ) )
            } else {
                None
            }
        },
        syn::Type::Reference( syn::TypeReference {
            lifetime: Some( ref lifetime ),
            mutability: None,
            ref elem,
            ..
        }) => {
            if is_bare_ty( &*elem, "str" ) {
                Some( Ty::RefStr( lifetime.clone() ) )
            } else if let Some( inner_ty ) = extract_slice_inner_ty( &*elem ) {
                if is_bare_ty( inner_ty, "u8" ) {
                    Some( Ty::RefSliceU8( lifetime.clone() ) )
                } else {
                    Some( Ty::RefSlice( lifetime.clone(), inner_ty.clone() ) )
                }
            } else {
                None
            }
        },
        _ => None
    }
}

fn get_fields< 'a, I: IntoIterator< Item = &'a syn::Field > + 'a >( fields: I ) -> Result< Vec< Field< 'a > >, syn::Error > {
    let mut length_until_eof_seen = false;
    let iter = fields.into_iter()
        .enumerate()
        .map( |(index, field)| {
            if length_until_eof_seen {
                let message = "The field with 'length = ..' has to be the last field; found another field";
                return Err( syn::Error::new( field.span(), message ) );
            }

            let mut default_on_eof = None;
            let mut length = None;
            let mut length_type = None;
            let mut skip = false;
            let mut varint = false;
            let mut constant_prefix = None;
            for attr in parse_attributes::< FieldAttribute >( &field.attrs )? {
                match attr {
                    FieldAttribute::DefaultOnEof { key_span } => {
                        if default_on_eof.is_some() {
                            let message = "Duplicate 'default_on_eof'";
                            return Err( syn::Error::new( key_span, message ) );
                        }

                        default_on_eof = Some( key_span );
                    }
                    FieldAttribute::Length { key_span, length_kind } => {
                        if length.is_some() {
                            let message = "Duplicate 'length'";
                            return Err( syn::Error::new( key_span, message ) );
                        }

                        if matches!( length_kind, LengthKind::UntilEndOfFile ) {
                            length_until_eof_seen = true;
                        }

                        length = Some( (key_span, length_kind) );
                    }
                    FieldAttribute::LengthType { key_span, ty } => {
                        if length_type.is_some() {
                            let message = "Duplicate 'length_type'";
                            return Err( syn::Error::new( key_span, message ) );
                        }

                        length_type = Some( (key_span, ty) );
                    },
                    FieldAttribute::Skip { key_span: _key_span } => {
                        skip = true;
                    },
                    FieldAttribute::ConstantPrefix { key_span, prefix } => {
                        if constant_prefix.is_some() {
                            let message = "Duplicate 'constant_prefix'";
                            return Err( syn::Error::new( key_span, message ) );
                        }
                        constant_prefix = Some( prefix );
                    },
                    FieldAttribute::VarInt { key_span } => {
                        if parse_primitive_ty( &field.ty ) != Some( PrimitiveTy::U64 ) {
                            let message = "The 'varint' attribute can only be used on fields of type 'u64'";
                            return Err( syn::Error::new( key_span, message ) );
                        }

                        varint = true;
                    }
                }
            }

            if let Some( ref value ) = constant_prefix {
                if value.value().is_empty() {
                    constant_prefix = None;
                }
            }

            if let (Some((key_span, _)), Some(_)) = (length_type, &length) {
                let message = "You cannot have both 'length_type' and 'length' on the same field";
                return Err( syn::Error::new( key_span, message ) );
            }

            let ty = if let Some( ty ) = extract_option_inner_ty( &field.ty ) {
                Opt::Option( parse_ty( ty ) )
            } else {
                Opt::Plain( parse_ty( &field.ty ) )
            };

            if length.is_some() {
                match ty {
                    | Opt::Plain( Ty::String )
                    | Opt::Plain( Ty::Vec( .. ) )
                    | Opt::Plain( Ty::CowSlice( .. ) )
                    | Opt::Plain( Ty::CowStr( .. ) )
                    | Opt::Plain( Ty::HashMap( .. ) )
                    | Opt::Plain( Ty::HashSet( .. ) )
                    | Opt::Plain( Ty::BTreeMap( .. ) )
                    | Opt::Plain( Ty::BTreeSet( .. ) )
                    | Opt::Plain( Ty::CowHashMap( .. ) )
                    | Opt::Plain( Ty::CowHashSet( .. ) )
                    | Opt::Plain( Ty::CowBTreeMap( .. ) )
                    | Opt::Plain( Ty::CowBTreeSet( .. ) )
                    | Opt::Plain( Ty::RefSliceU8( .. ) )
                    | Opt::Plain( Ty::RefSlice( .. ) )
                    | Opt::Plain( Ty::RefStr( .. ) )
                        => {},

                    | Opt::Option( Ty::String )
                    | Opt::Option( Ty::Vec( .. ) )
                    | Opt::Option( Ty::CowSlice( .. ) )
                    | Opt::Option( Ty::CowStr( .. ) )
                    | Opt::Option( Ty::HashMap( .. ) )
                    | Opt::Option( Ty::HashSet( .. ) )
                    | Opt::Option( Ty::BTreeMap( .. ) )
                    | Opt::Option( Ty::BTreeSet( .. ) )
                    | Opt::Option( Ty::CowHashMap( .. ) )
                    | Opt::Option( Ty::CowHashSet( .. ) )
                    | Opt::Option( Ty::CowBTreeMap( .. ) )
                    | Opt::Option( Ty::CowBTreeSet( .. ) )
                    | Opt::Option( Ty::RefSliceU8( .. ) )
                    | Opt::Option( Ty::RefSlice( .. ) )
                    | Opt::Option( Ty::RefStr( .. ) )
                    | Opt::Plain( Ty::Array( .. ) )
                    | Opt::Option( Ty::Array( .. ) )
                    | Opt::Plain( Ty::Primitive( .. ) )
                    | Opt::Option( Ty::Primitive( .. ) )
                    | Opt::Plain( Ty::Ty( .. ) )
                    | Opt::Option( Ty::Ty( .. ) )
                    => {
                        return Err(
                            syn::Error::new(
                                field.ty.span(),
                                "The 'length' attribute is only supported for `Vec`, `String`, `Cow<[_]>`, `Cow<str>`, `HashMap`, `HashSet`, `BTreeMap`, `BTreeSet`, `Cow<HashMap>`, `Cow<HashSet>`, `Cow<BTreeMap>`, `Cow<BTreeSet>`, `&[u8]`, `&str`"
                            )
                        );
                    }
                }
            }

            if length_type.is_some() {
                match ty {
                    | Opt::Plain( Ty::String )
                    | Opt::Plain( Ty::Vec( .. ) )
                    | Opt::Plain( Ty::CowSlice( .. ) )
                    | Opt::Plain( Ty::CowStr( .. ) )
                    | Opt::Plain( Ty::HashMap( .. ) )
                    | Opt::Plain( Ty::HashSet( .. ) )
                    | Opt::Plain( Ty::BTreeMap( .. ) )
                    | Opt::Plain( Ty::BTreeSet( .. ) )
                    | Opt::Plain( Ty::CowHashMap( .. ) )
                    | Opt::Plain( Ty::CowHashSet( .. ) )
                    | Opt::Plain( Ty::CowBTreeMap( .. ) )
                    | Opt::Plain( Ty::CowBTreeSet( .. ) )
                    | Opt::Plain( Ty::RefSliceU8( .. ) )
                    | Opt::Plain( Ty::RefSlice( .. ) )
                    | Opt::Plain( Ty::RefStr( .. ) )
                    | Opt::Option( Ty::String )
                    | Opt::Option( Ty::Vec( .. ) )
                    | Opt::Option( Ty::CowSlice( .. ) )
                    | Opt::Option( Ty::CowStr( .. ) )
                    | Opt::Option( Ty::HashMap( .. ) )
                    | Opt::Option( Ty::HashSet( .. ) )
                    | Opt::Option( Ty::BTreeMap( .. ) )
                    | Opt::Option( Ty::BTreeSet( .. ) )
                    | Opt::Option( Ty::CowHashMap( .. ) )
                    | Opt::Option( Ty::CowHashSet( .. ) )
                    | Opt::Option( Ty::CowBTreeMap( .. ) )
                    | Opt::Option( Ty::CowBTreeSet( .. ) )
                    | Opt::Option( Ty::RefSliceU8( .. ) )
                    | Opt::Option( Ty::RefSlice( .. ) )
                    | Opt::Option( Ty::RefStr( .. ) )
                        => {},

                    | Opt::Plain( Ty::Array( .. ) )
                    | Opt::Option( Ty::Array( .. ) )
                    | Opt::Plain( Ty::Primitive( .. ) )
                    | Opt::Option( Ty::Primitive( .. ) )
                    | Opt::Plain( Ty::Ty( .. ) )
                    | Opt::Option( Ty::Ty( .. ) )
                    => {
                        return Err(
                            syn::Error::new(
                                field.ty.span(),
                                "The 'length_type' attribute is only supported for `Vec`, `String`, `Cow<[_]>`, `Cow<str>`, `HashMap`, `HashSet`, `BTreeMap`, `BTreeSet`, `Cow<HashMap>`, `Cow<HashSet>`, `Cow<BTreeMap>`, `Cow<BTreeSet>`, `&[u8]`, `&str` and for `Option<T>` where `T` is one of these types"
                            )
                        );
                    }
                }
            }

            fn snd< T, U >( (_, b): (T, U) ) -> U {
                b
            }

            Ok( Field {
                index,
                name: field.ident.as_ref(),
                raw_ty: &field.ty,
                default_on_eof: default_on_eof.is_some(),
                length: length.map( snd ),
                length_type: length_type.map( snd ),
                ty,
                skip,
                varint,
                constant_prefix
            })
        });

    iter.collect()
}

fn default_on_eof_body( body: TokenStream ) -> TokenStream {
    quote! {
        match #body {
            Ok( value ) => value,
            Err( ref error ) if speedy::IsEof::is_eof( error ) => std::default::Default::default(),
            Err( error ) => return Err( error )
        }
    }
}

fn read_field_body( field: &Field ) -> TokenStream {
    if field.skip {
        return quote! {
            std::default::Default::default()
        };
    }

    let read_length_body = match field.length {
        Some( LengthKind::Expr( ref length ) ) => Some( quote! { ((#length) as usize) } ),
        Some( LengthKind::UntilEndOfFile ) => None,
        None => {
            let read_length_fn = match field.length_type.unwrap_or( DEFAULT_LENGTH_TYPE ) {
                BasicType::U7 => quote! { read_length_u7 },
                BasicType::U8 => quote! { read_length_u8 },
                BasicType::U16 => quote! { read_length_u16 },
                BasicType::U32 => quote! { read_length_u32 },
                BasicType::U64 => quote! { read_length_u64 },
                BasicType::VarInt64 => quote! { read_length_u64_varint },
            };

            let body = quote! {
                speedy::private::#read_length_fn( _reader_ )
            };

            if field.default_on_eof {
                Some( default_on_eof_body( body ) )
            } else {
                Some( quote! { #body? } )
            }
        }
    };

    let read_string = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_vec( _length_ ).and_then( speedy::private::vec_to_string )
            }}
        } else {
            quote! {{
                _reader_.read_vec_until_eof().and_then( speedy::private::vec_to_string )
            }}
        }
    };

    let read_vec = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_vec( _length_ )
            }}
        } else {
            quote! {{
                _reader_.read_vec_until_eof()
            }}
        }
    };

    let read_cow_slice = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_cow( _length_ )
            }}
        } else {
            quote! {{
                _reader_.read_cow_until_eof()
            }}
        }
    };

    let read_cow_str = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_cow( _length_ ).and_then( speedy::private::cow_bytes_to_cow_str )
            }}
        } else {
            quote! {{
                _reader_.read_cow_until_eof().and_then( speedy::private::cow_bytes_to_cow_str )
            }}
        }
    };

    let read_collection = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_collection( _length_ )
            }}
        } else {
            quote! {{
                _reader_.read_collection_until_eof()
            }}
        }
    };

    let read_key_value_collection = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_key_value_collection( _length_ )
            }}
        } else {
            quote! {{
                _reader_.read_collection_until_eof()
            }}
        }
    };

    let read_cow_collection = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_collection( _length_ ).map( std::borrow::Cow::Owned )
            }}
        } else {
            quote! {{
                _reader_.read_collection_until_eof().map( std::borrow::Cow::Owned )
            }}
        }
    };

    let read_cow_key_value_collection = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_key_value_collection( _length_ ).map( std::borrow::Cow::Owned )
            }}
        } else {
            quote! {{
                _reader_.read_collection_until_eof().map( std::borrow::Cow::Owned )
            }}
        }
    };

    let read_ref_slice_u8 = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                _reader_.read_bytes_borrowed( _length_ ).ok_or_else( speedy::private::error_unsized ).and_then( |error| error )
            }}
        } else {
            quote! {{
                _reader_.read_bytes_borrowed_until_eof().ok_or_else( speedy::private::error_unsized )
            }}
        }
    };

    let read_ref_str = || {
        if let Some( ref read_length_body ) = read_length_body {
            quote! {{
                let _length_ = #read_length_body;
                match _reader_.read_bytes_borrowed( _length_ ) {
                    Some( Ok( bytes ) ) => std::str::from_utf8( bytes ).map_err( speedy::private::error_invalid_str_utf8 ),
                    Some( Err( error ) ) => Err( error ),
                    None => Err( speedy::private::error_unsized() )
                }
            }}
        } else {
            quote! {{
                match _reader_.read_bytes_borrowed_until_eof() {
                    Some( bytes ) => std::str::from_utf8( bytes ).map_err( speedy::private::error_invalid_str_utf8 ),
                    None => Err( speedy::private::error_unsized() )
                }
            }}
        }
    };

    let read_ref_slice = |inner_ty: &syn::Type| {
        let inner;
        if let Some( ref read_length_body ) = read_length_body {
            inner = quote! {{
                let _length_ = #read_length_body;
                _length_.checked_mul( std::mem::size_of::< #inner_ty >() )
                    .ok_or_else( speedy::private::error_out_of_range_length )
                    .and_then( |bytelength| {
                        _reader_.read_bytes_borrowed( bytelength )
                            .ok_or_else( speedy::private::error_unsized )
                            .and_then( |error| error )
                    })
                    .map( |slice| unsafe { std::slice::from_raw_parts( slice.as_ptr() as *const #inner_ty, _length_ ) } )
            }}
        } else {
            inner = quote! {{
                _reader_.read_bytes_borrowed_until_eof()
                    .ok_or_else( speedy::private::error_unsized )
                    .map( |slice| unsafe {
                        std::slice::from_raw_parts( slice.as_ptr() as *const #inner_ty, slice.len() / std::mem::size_of::< #inner_ty >() )
                    })
            }}
        }

        quote! {{
            if std::mem::size_of::< #inner_ty >() != 1 && _reader_.endianness().conversion_necessary() {
                Err( speedy::private::error_endianness_mismatch() )
            } else {
                #inner
            }
        }}
    };

    let read_array = |length: u32| {
        // TODO: This is quite inefficient; for primitive types we can do better.
        let readers = (0..length).map( |_| quote! {
            match _reader_.read_value() {
                Ok( value ) => value,
                Err( error ) => return Err( error )
            }
        });

        quote! { (|| { Ok([
            #(#readers),*
        ])})() }
    };

    let read_u64_varint = || {
        quote! {{
            _reader_.read_u64_varint()
        }}
    };

    let read_option = |tokens: TokenStream|
        quote! {{
            _reader_.read_u8().and_then( |_flag_| {
                if _flag_ != 0 {
                    Ok( Some( #tokens? ) )
                } else {
                    Ok( None )
                }
            })
        }};

    let body = match field.ty.inner() {
        Ty::String => read_string(),
        Ty::Vec( .. ) => read_vec(),
        Ty::CowSlice( .. ) => read_cow_slice(),
        Ty::CowStr( .. ) => read_cow_str(),
        Ty::HashMap( .. ) |
        Ty::BTreeMap( .. )  => read_key_value_collection(),
        Ty::HashSet( .. ) |
        Ty::BTreeSet( .. ) => read_collection(),
        Ty::CowHashMap( .. ) |
        Ty::CowBTreeMap( .. ) => read_cow_key_value_collection(),
        Ty::CowHashSet( .. ) |
        Ty::CowBTreeSet( .. ) => read_cow_collection(),
        Ty::RefSliceU8( .. ) => read_ref_slice_u8(),
        Ty::RefSlice( _, inner_ty ) => read_ref_slice( inner_ty ),
        Ty::RefStr( .. ) => read_ref_str(),
        Ty::Array( _, length ) => read_array( *length ),
        Ty::Primitive( .. ) if field.varint => read_u64_varint(),
        Ty::Primitive( .. ) |
        Ty::Ty( .. ) => {
            assert!( field.length.is_none() );
            quote! { _reader_.read_value() }
        }
    };

    let body = match field.ty {
        Opt::Plain( _ ) => body,
        Opt::Option( _ ) => read_option( body )
    };

    let body = if let Some( ref constant_prefix ) = field.constant_prefix {
        quote! {{
            speedy::private::read_constant( _reader_, #constant_prefix ).and_then( |_| #body )
        }}
    } else {
        body
    };

    if field.default_on_eof {
        default_on_eof_body( body )
    } else {
        quote! { #body? }
    }
}

fn readable_body( types: &mut Vec< syn::Type >, st: &Struct ) -> (TokenStream, TokenStream, TokenStream) {
    let mut field_names = Vec::new();
    let mut field_readers = Vec::new();
    let mut minimum_bytes_needed = Vec::new();
    for field in &st.fields {
        let read_value = read_field_body( field );
        let name = field.var_name();
        let raw_ty = field.raw_ty;
        field_readers.push( quote! { let #name: #raw_ty = #read_value; } );
        field_names.push( name );
        types.extend( field.bound_types() );

        if let Some( minimum_bytes ) = get_minimum_bytes( field ) {
            minimum_bytes_needed.push( minimum_bytes );
        }
    }

    let body = quote! { #(#field_readers)* };
    let initializer = quote! { #(#field_names),* };
    let initializer = match st.kind {
        StructKind::Unit => initializer,
        StructKind::Unnamed => quote! { ( #initializer ) },
        StructKind::Named => quote! { { #initializer } }
    };

    let minimum_bytes_needed = sum( minimum_bytes_needed );
    (body, initializer, minimum_bytes_needed)
}

fn write_field_body( field: &Field ) -> TokenStream {
    let name = field.var_name();
    let write_length_body = match field.length {
        Some( LengthKind::Expr( ref length ) ) => {
            let field_name = format!( "{}", name );
            quote! {
                if !speedy::private::are_lengths_the_same( #name.len(), #length ) {
                    return Err( speedy::private::error_length_is_not_the_same_as_length_attribute( #field_name ) );
                }
            }
        },
        Some( LengthKind::UntilEndOfFile ) => quote! {},
        None => {
            let write_length_fn = match field.length_type.unwrap_or( DEFAULT_LENGTH_TYPE ) {
                BasicType::U7 => quote! { write_length_u7 },
                BasicType::U8 => quote! { write_length_u8 },
                BasicType::U16 => quote! { write_length_u16 },
                BasicType::U32 => quote! { write_length_u32 },
                BasicType::U64 => quote! { write_length_u64 },
                BasicType::VarInt64 => quote! { write_length_u64_varint }
            };

            quote! { speedy::private::#write_length_fn( #name.len(), _writer_ )?; }
        }
    };

    let write_str = ||
        quote! {{
            #write_length_body
            _writer_.write_slice( #name.as_bytes() )?;
        }};

    let write_slice = ||
        quote! {{
            #write_length_body
            _writer_.write_slice( &#name )?;
        }};

    let write_collection = ||
        quote! {{
            #write_length_body
            _writer_.write_collection( #name.iter() )?;
        }};

    let write_array = ||
        quote! {{
            _writer_.write_slice( &#name[..] )?;
        }};

    let write_u64_varint = ||
        quote! {{
            _writer_.write_u64_varint( *#name )?;
        }};

    let write_option = |tokens: TokenStream|
        quote! {{
            if let Some( ref #name ) = #name {
                _writer_.write_u8( 1 )?;
                #tokens
            } else {
                _writer_.write_u8( 0 )?;
            }
        }};

    let body = match field.ty.inner() {
        Ty::String |
        Ty::CowStr( .. ) |
        Ty::RefStr( .. )
            => write_str(),
        Ty::Vec( .. ) |
        Ty::CowSlice( .. ) |
        Ty::RefSliceU8( .. ) |
        Ty::RefSlice( .. )
            => write_slice(),
        Ty::HashMap( .. ) |
        Ty::HashSet( .. ) |
        Ty::BTreeMap( .. ) |
        Ty::BTreeSet( .. ) |
        Ty::CowHashMap( .. ) |
        Ty::CowHashSet( .. ) |
        Ty::CowBTreeMap( .. ) |
        Ty::CowBTreeSet( .. ) => write_collection(),
        Ty::Array( .. ) => write_array(),
        Ty::Primitive( .. ) if field.varint => write_u64_varint(),
        Ty::Primitive( .. ) |
        Ty::Ty( .. ) => {
            assert!( field.length.is_none() );
            quote! { _writer_.write_value( #name )?; }
        }
    };

    let body = match field.ty {
        Opt::Plain( _ ) => body,
        Opt::Option( _ ) => write_option( body )
    };

    let body = if let Some( ref constant_prefix ) = field.constant_prefix {
        quote! {{
            _writer_.write_slice( #constant_prefix )?;
            #body
        }}
    } else {
        body
    };

    body
}

fn writable_body( types: &mut Vec< syn::Type >, st: &Struct ) -> (TokenStream, TokenStream) {
    let mut field_names = Vec::new();
    let mut field_writers = Vec::new();
    for field in &st.fields {
        if field.skip {
            continue;
        }

        let write_value = write_field_body( field );
        types.extend( field.bound_types() );

        field_names.push( field.var_name().clone() );
        field_writers.push( write_value );
    }

    let body = quote! { #(#field_writers)* };
    let initializer = quote! { #(ref #field_names),* };
    let initializer = match st.kind {
        StructKind::Unit => initializer,
        StructKind::Unnamed => quote! { ( #initializer ) },
        StructKind::Named => quote! { { #initializer } }
    };

    (body, initializer)
}

struct Variant< 'a > {
    tag_expr: TokenStream,
    ident: &'a syn::Ident,
    structure: Struct< 'a >
}

struct Enum< 'a > {
    tag_type: BasicType,
    peek_tag: bool,
    variants: Vec< Variant< 'a > >
}

impl< 'a > Enum< 'a > {
    fn new(
        ident: &syn::Ident,
        attrs: &[syn::Attribute],
        raw_variants: &'a Punctuated< syn::Variant, syn::token::Comma >
    ) -> Result< Self, syn::Error > {
        let attrs = parse_attributes::< EnumAttribute >( attrs )?;
        let attrs = collect_enum_attributes( attrs )?;
        let tag_type = attrs.tag_type.unwrap_or( DEFAULT_ENUM_TAG_TYPE );
        let max = match tag_type {
            BasicType::U7 => 0b01111111_u64,
            BasicType::U8 => std::u8::MAX as u64,
            BasicType::U16 => std::u16::MAX as u64,
            BasicType::U32 => std::u32::MAX as u64,
            BasicType::U64 => std::u64::MAX,
            BasicType::VarInt64 => std::u64::MAX
        };

        let mut previous_tag = None;
        let mut tag_to_full_name = HashMap::new();
        let mut variants = Vec::new();
        for variant in raw_variants {
            let mut struct_attrs = Vec::new();
            let mut variant_attrs = Vec::new();
            for attr in parse_attributes::< VariantOrStructAttribute >( &variant.attrs )? {
                match attr {
                    VariantOrStructAttribute::Struct( attr ) => struct_attrs.push( attr ),
                    VariantOrStructAttribute::Variant( attr ) => variant_attrs.push( attr )
                }
            }

            let variant_attrs = collect_variant_attributes( variant_attrs )?;

            let full_name = format!( "{}::{}", ident, variant.ident );
            let tag = if let Some( tag ) = variant_attrs.tag {
                if tag > max {
                    let message = format!( "Enum discriminant `{}` is too big!", full_name );
                    return Err( syn::Error::new( variant.span(), message ) );
                }
                tag
            } else {
                match variant.discriminant {
                    None => {
                        let tag = if let Some( previous_tag ) = previous_tag {
                            if previous_tag >= max {
                                let message = format!( "Enum discriminant `{}` is too big!", full_name );
                                return Err( syn::Error::new( variant.span(), message ) );
                            }

                            previous_tag + 1
                        } else {
                            0
                        };

                        tag
                    },
                    Some( (_, syn::Expr::Lit( syn::ExprLit { lit: syn::Lit::Int( ref raw_value ), .. } )) ) => {
                        let tag = raw_value.base10_parse::< u64 >().map_err( |err| {
                            syn::Error::new( raw_value.span(), err )
                        })?;
                        if tag > max {
                            let message = format!( "Enum discriminant `{}` is too big!", full_name );
                            return Err( syn::Error::new( raw_value.span(), message ) );
                        }

                        tag
                    },
                    Some((_, ref expr)) => {
                        let message = format!( "Enum discriminant `{}` is currently unsupported!", full_name );
                        return Err( syn::Error::new( expr.span(), message ) );
                    }
                }
            };

            previous_tag = Some( tag );
            if let Some( other_full_name ) = tag_to_full_name.get( &tag ) {
                let message = format!( "Two discriminants with the same value of '{}': `{}`, `{}`", tag, full_name, other_full_name );
                return Err( syn::Error::new( variant.span(), message ) );
            }

            tag_to_full_name.insert( tag, full_name );
            let tag_expr = match tag_type {
                BasicType::U7 | BasicType::U8 => {
                    let tag = tag as u8;
                    quote! { #tag }
                },
                BasicType::U16 => {
                    let tag = tag as u16;
                    quote! { #tag }
                },
                BasicType::U32 => {
                    let tag = tag as u32;
                    quote! { #tag }
                },
                BasicType::U64 | BasicType::VarInt64 => {
                    quote! { #tag }
                }
            };

            let struct_attrs = collect_struct_attributes( struct_attrs )?;
            let structure = Struct::new( &variant.fields, &struct_attrs )?;
            variants.push( Variant {
                tag_expr,
                ident: &variant.ident,
                structure
            });
        }

        Ok( Enum {
            tag_type,
            peek_tag: attrs.peek_tag,
            variants
        })
    }
}

fn get_minimum_bytes( field: &Field ) -> Option< TokenStream > {
    if field.default_on_eof || field.length.is_some() || field.skip {
        None
    } else {
        let mut length = match field.ty {
            Opt::Option( .. ) => {
                quote! { 1 }
            },
            Opt::Plain( ref ty ) => {
                match ty {
                    | Ty::String
                    | Ty::Vec( .. )
                    | Ty::CowSlice( .. )
                    | Ty::CowStr( .. )
                    | Ty::HashMap( .. )
                    | Ty::HashSet( .. )
                    | Ty::BTreeMap( .. )
                    | Ty::BTreeSet( .. )
                    | Ty::CowHashMap( .. )
                    | Ty::CowHashSet( .. )
                    | Ty::CowBTreeMap( .. )
                    | Ty::CowBTreeSet( .. )
                    | Ty::RefSliceU8( .. )
                    | Ty::RefSlice( .. )
                    | Ty::RefStr( .. )
                    => {
                        let size: usize = match field.length_type.unwrap_or( DEFAULT_LENGTH_TYPE ) {
                            BasicType::U7 | BasicType::U8 | BasicType::VarInt64 => 1,
                            BasicType::U16 => 2,
                            BasicType::U32 => 4,
                            BasicType::U64 => 8
                        };

                        quote! { #size }
                    },
                    | Ty::Array( ty, length ) => {
                        let length = *length as usize;
                        quote! { <#ty as speedy::Readable< 'a_, C_ >>::minimum_bytes_needed() * #length }
                    },
                    | Ty::Primitive( .. ) if field.varint => quote! { 1 },
                    | Ty::Primitive( .. )
                    | Ty::Ty( .. ) => {
                        let raw_ty = &field.raw_ty;
                        quote! { <#raw_ty as speedy::Readable< 'a_, C_ >>::minimum_bytes_needed() }
                    }
                }
            }
        };

        if let Some( ref constant_prefix ) = field.constant_prefix {
            let extra_length = constant_prefix.value().len();
            length = quote! { #length + #extra_length };
        }

        Some( length )
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

fn generate_is_primitive( fields: &[Field], is_writable: bool, check_order: bool, is_forced: bool ) -> TokenStream {
    if fields.is_empty() || is_forced {
        return quote! { true };
    }

    let mut is_primitive = Vec::new();
    let mut fields_size = Vec::new();
    let mut fields_offsets = Vec::new();
    for field in fields {
        if !is_primitive.is_empty() {
            is_primitive.push( quote! { && });
            fields_size.push( quote! { + });
        }

        let ty = &field.raw_ty;
        if is_writable {
            is_primitive.push( quote! {
                <#ty as speedy::Writable< C_ >>::speedy_is_primitive()
            });
        } else {
            is_primitive.push( quote! {
                <#ty as speedy::Readable< 'a_, C_ >>::speedy_is_primitive()
            });
        }

        fields_size.push( quote! {
            std::mem::size_of::< #ty >()
        });

        let name = field.name();
        fields_offsets.push( quote! {
            speedy::private::offset_of!( Self, #name )
        });
    }

    is_primitive.push( quote! {
        && (#(#fields_size)*) == std::mem::size_of::< Self >()
    });

    if check_order {
        for window in fields_offsets.windows( 2 ) {
            let a = &window[0];
            let b = &window[1];
            is_primitive.push( quote! {
                && #a <= #b
            });
        }
    }

    quote! {
        #(#is_primitive)*
    }
}

struct IsPrimitiveResult {
    is_ty_packed: bool,
    is_ty_transparent: bool,
    is_ty_potentially_primitive: bool,
    check_order: bool,
    impl_zerocopyable: bool,
    is_forced: bool
}

fn check_is_primitive( input: &syn::DeriveInput, structure: &Struct, attrs: &ToplevelStructAttributes ) -> IsPrimitiveResult {
    let is_ty_packed = is_packed( &input.attrs );
    let is_ty_transparent = structure.fields.len() == 1 && is_transparent( &input.attrs );
    let is_ty_c = is_c( &input.attrs );
    let is_ty_simple = structure.fields.iter().all( |field| field.is_simple() );
    let check_order = !is_ty_transparent && !is_ty_packed && !is_ty_c;

    let mut is_forced = false;
    match attrs.is_primitive {
        None => {},
        Some( IsPrimitive::Always ) => {
            is_forced = true;
        }
    }

    let can_be_primitive = is_forced || structure.fields.iter().all( |field| field.can_be_primitive() );
    IsPrimitiveResult {
        is_ty_packed,
        is_ty_transparent,
        is_ty_potentially_primitive: can_be_primitive && (is_forced || is_ty_transparent || is_ty_packed || is_ty_c || is_ty_simple || (!uses_generics( &input ) && structure.fields.len() <= 4)),
        check_order,
        impl_zerocopyable: is_ty_packed || is_ty_transparent || is_forced,
        is_forced
    }
}

fn impl_readable( input: syn::DeriveInput ) -> Result< TokenStream, syn::Error > {
    let name = &input.ident;
    let mut types = Vec::new();

    let (reader_body, minimum_bytes_needed_body, impl_primitive, impl_zerocopyable) = match &input.data {
        syn::Data::Struct( syn::DataStruct { ref fields, .. } ) => {
            let attrs = parse_attributes::< ToplevelStructAttribute >( &input.attrs )?;
            let attrs = collect_toplevel_struct_attributes( attrs )?;
            let structure = Struct::new( fields, &attrs )?;
            let is_primitive = check_is_primitive( &input, &structure, &attrs );
            let (body, initializer, minimum_bytes) = readable_body( &mut types, &structure );
            let reader_body = quote! {
                #body
                Ok( #name #initializer )
            };

            let mut field_types = Vec::new();
            for field in &structure.fields {
                field_types.push( field.raw_ty.clone() );
            }

            let impl_primitive = if is_primitive.is_ty_transparent {
                let field_ty = &structure.fields[ 0 ].raw_ty;
                quote! {
                    #[inline(always)]
                    fn speedy_is_primitive() -> bool {
                        <#field_ty as speedy::Readable< 'a_, C_ >>::speedy_is_primitive()
                    }

                    #[inline]
                    unsafe fn speedy_slice_from_bytes( slice: &[u8] ) -> &[Self] {
                        let slice = <#field_ty as speedy::Readable< 'a_, C_ >>::speedy_slice_from_bytes( slice );
                        std::slice::from_raw_parts( slice.as_ptr() as *const Self, slice.len() )
                    }

                    #[inline(always)]
                    fn speedy_convert_slice_endianness( endianness: speedy::Endianness, slice: &mut [Self] ) {
                        unsafe {
                            let slice = std::slice::from_raw_parts_mut( slice.as_mut_ptr() as *mut #field_ty, slice.len() );
                            <#field_ty as speedy::Readable< 'a_, C_ >>::speedy_convert_slice_endianness( endianness, slice )
                        }
                    }
                }
            } else if is_primitive.is_ty_potentially_primitive {
                let is_primitive = generate_is_primitive( &structure.fields, false, is_primitive.check_order, is_primitive.is_forced );
                let mut body_flip_endianness = Vec::new();
                for field in &structure.fields {
                    let ty = &field.raw_ty;
                    let name = field.name();

                    body_flip_endianness.push( quote! {
                        unsafe {
                            <#ty as speedy::Readable< 'a_, C_ >>::speedy_flip_endianness(
                                std::ptr::addr_of_mut!( (*itself).#name )
                            );
                        }
                    });
                }

                quote! {
                    #[inline(always)]
                    fn speedy_is_primitive() -> bool {
                        #is_primitive
                    }

                    #[inline]
                    unsafe fn speedy_slice_from_bytes( slice: &[u8] ) -> &[Self] {
                        unsafe {
                            std::slice::from_raw_parts( slice.as_ptr() as *const Self, slice.len() / std::mem::size_of::< Self >() )
                        }
                    }

                    #[inline(always)]
                    unsafe fn speedy_flip_endianness( itself: *mut Self ) {
                        unsafe {
                            #(#body_flip_endianness)*
                        }
                    }

                    #[inline(always)]
                    fn speedy_convert_slice_endianness( endianness: speedy::Endianness, slice: &mut [Self] ) {
                        if endianness.conversion_necessary() {
                            for value in slice {
                                unsafe {
                                    <Self as speedy::Readable< 'a_, C_ >>::speedy_flip_endianness( value );
                                }
                            }
                        }
                    }
                }
            } else {
                quote! {}
            };

            let impl_zerocopyable = if is_primitive.impl_zerocopyable {
                let (impl_params, ty_params, where_clause) = common_tokens( &input, &field_types, Trait::ZeroCopyable { is_packed: is_primitive.is_ty_packed, is_forced: is_primitive.is_forced } );
                quote! {
                    unsafe impl< #impl_params C_, T_ > speedy::private::ZeroCopyable< C_, T_ > for #name #ty_params #where_clause {}
                }
            } else {
                quote! {}
            };

            (reader_body, minimum_bytes, impl_primitive, impl_zerocopyable)
        },
        syn::Data::Enum( syn::DataEnum { variants, .. } ) => {
            let enumeration = Enum::new( name, &input.attrs, variants )?;
            let mut variant_matches = Vec::with_capacity( variants.len() );
            let mut variant_minimum_sizes = Vec::with_capacity( variants.len() );
            for variant in enumeration.variants {
                let tag = variant.tag_expr;
                let unqualified_ident = &variant.ident;
                let variant_path = quote! { #name::#unqualified_ident };
                let (body, initializer, minimum_bytes) = readable_body( &mut types, &variant.structure );
                variant_matches.push( quote! {
                    #tag => {
                        #body
                        Ok( #variant_path #initializer )
                    }
                });

                if variant.structure.kind != StructKind::Unit && variant.structure.is_guaranteed_non_recursive() {
                    variant_minimum_sizes.push( minimum_bytes );
                }
            }

            let tag_size = match enumeration.tag_type {
                BasicType::U64 => 8_usize,
                BasicType::U32 => 4_usize,
                BasicType::U16 => 2_usize,
                BasicType::U8 => 1_usize,
                BasicType::U7 => 1_usize,
                BasicType::VarInt64 => 1_usize,
            };

            let tag_reader = match (enumeration.peek_tag, enumeration.tag_type) {
                (false, BasicType::U64) => quote! { read_u64 },
                (false, BasicType::U32) => quote! { read_u32 },
                (false, BasicType::U16) => quote! { read_u16 },
                (false, BasicType::U8) => quote! { read_u8 },
                (false, BasicType::U7) => quote! { read_u8 },
                (false, BasicType::VarInt64) => quote! { read_u64_varint },

                (true, BasicType::U64) => quote! { peek_u64 },
                (true, BasicType::U32) => quote! { peek_u32 },
                (true, BasicType::U16) => quote! { peek_u16 },
                (true, BasicType::U8) => quote! { peek_u8 },
                (true, BasicType::U7) => quote! { peek_u8 },
                (true, BasicType::VarInt64) => quote! { peek_u64_varint },
            };

            let reader_body = quote! {
                let kind_ = _reader_.#tag_reader()?;
                match kind_ {
                    #(#variant_matches),*
                    _ => Err( speedy::private::error_invalid_enum_variant() )
                }
            };
            let minimum_bytes_needed_body = min( variant_minimum_sizes.into_iter() );
            let minimum_bytes_needed_body =
                if !enumeration.peek_tag {
                    quote! { (#minimum_bytes_needed_body) + #tag_size }
                } else {
                    quote! { std::cmp::max( #minimum_bytes_needed_body, #tag_size ) }
                };

            (reader_body, minimum_bytes_needed_body, quote! {}, quote! {})
        },
        syn::Data::Union( syn::DataUnion { union_token, .. } ) => {
            let message = "Unions are not supported!";
            return Err( syn::Error::new( union_token.span(), message ) );
        }
    };

    let (impl_params, ty_params, where_clause) = common_tokens( &input, &types, Trait::Readable );
    let output = quote! {
        impl< 'a_, #impl_params C_: speedy::Context > speedy::Readable< 'a_, C_ > for #name #ty_params #where_clause {
            #[inline]
            fn read_from< R_: speedy::Reader< 'a_, C_ > >( _reader_: &mut R_ ) -> std::result::Result< Self, C_::Error > {
                #reader_body
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                #minimum_bytes_needed_body
            }

            #impl_primitive
        }

        #impl_zerocopyable
    };

    Ok( output )
}

fn assign_to_variables< 'a >( fields: impl IntoIterator< Item = &'a Field< 'a > >, is_packed: bool ) -> TokenStream {
    let fields: Vec< _ > = fields.into_iter().filter(|field| !field.skip).map( |field| {
        let var_name = field.var_name();
        let name = field.name();

        if !is_packed {
            quote! {
                let #var_name = &self.#name;
            }
        } else {
            quote! {
                let #var_name = self.#name;
                let #var_name = &#var_name;
            }
        }
    }).collect();

    quote! {
        #(#fields)*
    }
}

fn impl_writable( input: syn::DeriveInput ) -> Result< TokenStream, syn::Error > {
    let name = &input.ident;
    let mut types = Vec::new();
    let (writer_body, impl_primitive) = match input.data {
        syn::Data::Struct( syn::DataStruct { ref fields, .. } ) => {
            let attrs = parse_attributes::< ToplevelStructAttribute >( &input.attrs )?;
            let attrs = collect_toplevel_struct_attributes( attrs )?;
            let st = Struct::new( fields, &attrs )?;
            let is_primitive = check_is_primitive( &input, &st, &attrs );
            let assignments = assign_to_variables( &st.fields, is_primitive.is_ty_packed );
            let (body, _) = writable_body( &mut types, &st );

            let impl_primitive =
                if is_primitive.is_ty_transparent {
                    let field_ty = &st.fields[ 0 ].raw_ty;
                    quote! {
                        #[inline(always)]
                        fn speedy_is_primitive() -> bool {
                            <#field_ty as speedy::Writable< C_ >>::speedy_is_primitive()
                        }

                        #[inline(always)]
                        unsafe fn speedy_slice_as_bytes( slice: &[Self] ) -> &[u8] where Self: Sized {
                            unsafe {
                                std::slice::from_raw_parts( slice.as_ptr() as *const u8, slice.len() * std::mem::size_of::< Self >() )
                            }
                        }
                    }

                } else if is_primitive.is_ty_potentially_primitive {
                    let is_primitive = generate_is_primitive( &st.fields, true, is_primitive.check_order, is_primitive.is_forced );
                    quote! {
                        #[inline(always)]
                        fn speedy_is_primitive() -> bool {
                            #is_primitive
                        }

                        #[inline(always)]
                        unsafe fn speedy_slice_as_bytes( slice: &[Self] ) -> &[u8] where Self: Sized {
                            unsafe {
                                std::slice::from_raw_parts( slice.as_ptr() as *const u8, slice.len() * std::mem::size_of::< Self >() )
                            }
                        }
                    }
                } else {
                    quote! {}
                };

            let impl_body = quote! {
                #assignments
                #body
            };

            (impl_body, impl_primitive)
        },
        syn::Data::Enum( syn::DataEnum { ref variants, .. } ) => {
            let enumeration = Enum::new( name, &input.attrs, variants )?;
            let tag_writer = match enumeration.tag_type {
                BasicType::U64 => quote! { write_u64 },
                BasicType::U32 => quote! { write_u32 },
                BasicType::U16 => quote! { write_u16 },
                BasicType::U8 => quote! { write_u8 },
                BasicType::U7 => quote! { write_u8 },
                BasicType::VarInt64 => quote! { write_u64_varint },
            };

            let variants: Result< Vec< _ >, syn::Error > = enumeration.variants.iter()
                .map( |variant| {
                    let unqualified_ident = &variant.ident;
                    let tag_expr = &variant.tag_expr;
                    let variant_path = quote! { #name::#unqualified_ident };
                    let (body, initializer) = writable_body( &mut types, &variant.structure );
                    let write_tag =
                        if !enumeration.peek_tag {
                            quote! { _writer_.#tag_writer( #tag_expr )?; }
                        } else {
                            quote! {}
                        };

                    let snippet = quote! {
                        #variant_path #initializer => {
                            #write_tag
                            #body
                        }
                    };

                    Ok( snippet )
                })
                .collect();
            let variants = variants?;
            (quote! { match *self { #(#variants),* } }, quote! {})
        },
        syn::Data::Union( syn::DataUnion { union_token, .. } ) => {
            let message = "Unions are not supported!";
            return Err( syn::Error::new( union_token.span(), message ) );
        }
    };

    let (impl_params, ty_params, where_clause) = common_tokens( &input, &types, Trait::Writable );
    let output = quote! {
        impl< #impl_params C_: speedy::Context > speedy::Writable< C_ > for #name #ty_params #where_clause {
            #[inline]
            fn write_to< T_: ?Sized + speedy::Writer< C_ > >( &self, _writer_: &mut T_ ) -> std::result::Result< (), C_::Error > {
                #writer_body
                Ok(())
            }

            #impl_primitive
        }
    };

    Ok( output )
}
