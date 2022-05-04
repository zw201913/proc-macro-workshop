use proc_macro::TokenStream;
use std::collections::HashMap;
use proc_macro2::Ident;
use syn;
use syn::spanned::Spanned;
use quote;
use syn::{Error, Field, PathSegment};
use syn::punctuated::Punctuated;
use syn::Token;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = syn::parse_macro_input!(input as syn::DeriveInput);
    // eprintln!("{:#?}", &derive_input);
    match do_expand(&derive_input) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into()
    }
}


fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = st.ident.to_string();
    let builder_name = format!("{}Builder", struct_name);
    let builder_ident = syn::Ident::new(&builder_name, st.span());
    let struct_ident = &st.ident;

    // let (option_res, none_res, init_setter_tokenstream, new_object_tokenstream) = gererate_builder_struct_fields_def(st);

    match gererate_builder_struct_fields_def(st) {
        Ok((option_res, none_res, init_setter_tokenstream, new_object_tokenstream)) => {
            let struct_fields = option_res;
            let none_files = none_res;

            let attr_functions = init_setter_tokenstream;

            let build_function = new_object_tokenstream;

            // let build_function = generate_build_functions(st).unwrap();

            let res = quote::quote!(
                pub struct #builder_ident{
                    #struct_fields
                }
                impl #struct_ident{
                    pub fn builder()->#builder_ident{
                        #builder_ident{
                            #none_files
                        }
                    }
                }
                impl #builder_ident{
                    #attr_functions
                    #build_function
                }
            );
            Ok(res)
        }
        Err(e) => {
            return Err(e);
        }
    }
}

type StructFields = Punctuated<Field, Token![,]>;

fn get_fields_from_driver_input(st: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }), .. }) = st.data {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(st, "Must define on struct,Not on enum".to_string()))
}


struct FieldMetaInfo<'a> {
    name: &'a Option<syn::Ident>,
    is_optional: bool,
    is_array: bool,
    ty: Option<&'a syn::Type>,
    each_name: Option<syn::Ident>,
}

impl<'a> FieldMetaInfo<'a> {
    pub fn new() -> Self {
        FieldMetaInfo {
            name: &None,
            is_optional: false,
            is_array: false,
            ty: None,
            each_name: None,
        }
    }


    pub fn name(&mut self, name: &'a Option<syn::Ident>) {
        self.name = name;
    }

    pub fn is_optional(&mut self, is_optional: bool) {
        self.is_optional = is_optional;
    }

    pub fn is_array(&mut self, is_array: bool) {
        self.is_array = is_array;
    }

    pub fn ty(&mut self, ty: &'a syn::Type) {
        self.ty = Some(ty);
    }

    pub fn each_name(&mut self, each_name: &str, span: proc_macro2::Span) {
        self.each_name = Some(syn::Ident::new(each_name, span));
    }

    pub fn init_builder_struct(&self) -> proc_macro2::TokenStream {
        let ident = self.name;
        let ty = self.ty;
        if self.is_array {
            quote::quote!(
                #ident:std::vec::Vec<#ty>,
            )
        } else {
            quote::quote!(
                #ident:std::option::Option<#ty>,
            )
        }
    }

    pub fn init_builder(&self) -> proc_macro2::TokenStream {
        let ident = self.name;
        if self.is_array {
            quote::quote!(
                #ident:std::vec::Vec::new(),
            )
        } else {
            quote::quote!(
                #ident:std::option::Option::None,
            )
        }
    }

    pub fn init_setter_fn(&self) -> proc_macro2::TokenStream {
        let ty = self.ty;
        let each_name = &self.each_name;
        let ident = self.name;
        if each_name.is_none() {
            if self.is_array {
                quote::quote!(
                    pub fn #ident(&mut self, #ident:std::vec::Vec<#ty>)->&mut Self{
                        self.#ident.extend(#ident);
                        self
                    }
                )
            } else {
                quote::quote!(
                    pub fn #ident(&mut self, #ident:#ty)->&mut Self{
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                )
            }
        } else {
            quote::quote!(
                pub fn #each_name(&mut self, #each_name:#ty)->&mut Self{
                    self.#ident.push(#each_name);
                    self
                }
            )
        }
    }

    pub fn check_fields_tokenstream(&self) -> proc_macro2::TokenStream {
        let ident = self.name;
        if self.is_optional || self.is_array {
            proc_macro2::TokenStream::new()
        } else {
            quote::quote!(
                if self.#ident.is_none(){
                    let err = format!("{} is missing",stringify!(#ident));
                    return std::result::Result::Err(err.into());
                }
            )
        }
    }

    pub fn init_new_object(&self) -> proc_macro2::TokenStream {
        let ident = self.name;
        if self.is_optional || self.is_array {
            quote::quote!(
                #ident:self.#ident.clone(),
            )
        } else {
            quote::quote!(
                #ident:self.#ident.clone().unwrap(),
            )
        }
    }
}

fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<FieldMetaInfo> {
    let mut field_meta_info = FieldMetaInfo::new();
    field_meta_info.name(&field.ident);
    field_meta_info.ty(&field.ty);
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) = &field.ty {
        if let Some(syn::PathSegment { ident, arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }) }) = segments.last() {
            if ident.to_string() == "Option" {
                if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                    field_meta_info.ty(inner_type);
                }
                field_meta_info.is_optional(true);
            } else if ident.to_string() == "Vec" {
                if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                    field_meta_info.ty(inner_type);
                }
                field_meta_info.is_array(true);
            }
        }
    }
    for attr in &field.attrs {
        if let Ok(syn::Meta::List(ref list)) = attr.parse_meta() {
            if let syn::MetaList { ref path, ref nested, .. } = list {
                if let Some(path_segment) = path.segments.first() {
                    if path_segment.ident == "builder" {
                        if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue { ref path, ref lit, .. }))) = nested.first() {
                            if path.is_ident("each") {
                                if let syn::Lit::Str(ref lit_str) = lit {
                                    field_meta_info.each_name(&lit_str.value(), lit.span());
                                }
                            } else {
                                return Err(syn::Error::new_spanned(list, r#"expected `builder(each = "...")`"#));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(field_meta_info)
}

fn gererate_builder_struct_fields_def(st: &syn::DeriveInput) -> syn::Result<(proc_macro2::TokenStream, proc_macro2::TokenStream, proc_macro2::TokenStream, proc_macro2::TokenStream)> {
    let fields = get_fields_from_driver_input(st).unwrap();
    let mut init_builder_struct_tokenstream = proc_macro2::TokenStream::new();
    let mut init_builder_tokenstream = proc_macro2::TokenStream::new();
    let mut init_setter_tokenstream = proc_macro2::TokenStream::new();
    let mut check_fields_tokenstream = proc_macro2::TokenStream::new();
    let mut new_object_tokenstream = proc_macro2::TokenStream::new();
    for field in fields {
        match get_user_specified_ident_for_vec(field) {
            Ok(field_meta_info) => {
                init_builder_struct_tokenstream.extend(field_meta_info.init_builder_struct());
                init_builder_tokenstream.extend(field_meta_info.init_builder());
                init_setter_tokenstream.extend(field_meta_info.init_setter_fn());
                check_fields_tokenstream.extend(field_meta_info.check_fields_tokenstream());
                new_object_tokenstream.extend(field_meta_info.init_new_object())
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
    let generate_command_object_tokenstream =
        quote::quote!(
        pub fn build(&mut self)->std::result::Result<Command, std::boxed::Box<dyn std::error::Error>>{
            #check_fields_tokenstream
            std::result::Result::Ok(Command{#new_object_tokenstream})
        }
    );
    eprintln!("{:#?}", init_setter_tokenstream);
    Ok((init_builder_struct_tokenstream, init_builder_tokenstream, init_setter_tokenstream, generate_command_object_tokenstream))
}

fn get_fields_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) = ty {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == "Option" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}

fn gererate_fields_fn(st: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let fields = get_fields_from_driver_input(st).unwrap();
    let mut field_functions_tokenstream = proc_macro2::TokenStream::new();
    // let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    // let types_: Vec<_> = fields.iter().map(|f| &f.ty).collect();
    // for (ident, type_) in idents.iter().zip(types_.iter()) {
    //     eprintln!("{:#?}", ident);
    //     eprintln!("{:#?}", type_);
    //     field_functions_tokenstream.extend(quote::quote!(
    //         pub fn #ident(&mut self,#ident:#type_)->&mut Self{
    //             self.#ident = #ident;
    //             self
    //         }
    //     ));
    // }
    //
    fields.iter().for_each(|f| {
        let ident = &f.ident;
        if let Some(inner_type) = get_fields_type(&f.ty) {
            field_functions_tokenstream.extend(quote::quote!(
            pub fn #ident(&mut self,#ident:#inner_type)->&mut Self{
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        ));
        } else {
            let type_ = &f.ty;
            field_functions_tokenstream.extend(quote::quote!(
            pub fn #ident(&mut self,#ident:#type_)->&mut Self{
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        ));
        }
    });
    // eprintln!("{:#?}",field_functions_tokenstream);
    field_functions_tokenstream
}

fn generate_build_functions(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = &st.ident;
    let fields = get_fields_from_driver_input(st).unwrap();
    let mut check_fields_tokenstream = proc_macro2::TokenStream::new();
    let mut fill_fields_tokenstream = proc_macro2::TokenStream::new();
    fields.iter().for_each(|f| {
        let ident = &f.ident;
        if let Some(_) = get_fields_type(&f.ty) {
            fill_fields_tokenstream.extend(quote::quote!(
                #ident:self.#ident.clone(),
            ));
        } else {
            check_fields_tokenstream.extend(quote::quote!(
                if self.#ident.is_none(){
                    let err = format!("{} is missing",stringify!(#ident));
                    return std::result::Result::Err(err.into());
                };
            ));
            fill_fields_tokenstream.extend(quote::quote!(
                #ident:self.#ident.clone().unwrap(),
            ));
        }
    });
    Ok(quote::quote!(
        pub fn build(&mut self)->std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>>{
            #check_fields_tokenstream
            let result = #struct_name{
                #fill_fields_tokenstream
            };
            std::result::Result::Ok(result)
        }
    ))
}