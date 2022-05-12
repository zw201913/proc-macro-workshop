use proc_macro::TokenStream;
use std::collections::HashMap;
use quote;
use syn::TypePath;
use syn::visit::Visit;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&derive_input) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into()
    }
}

fn do_expand(derive_input: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &derive_input.ident;
    let name_str = &name.to_string();
    match get_fields(&derive_input) {
        Ok(fields) => {
            let fields_token_stream = gerenate_fields_tokenstream(fields);
            let generics = get_generics(derive_input);
            let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
            Ok(quote::quote!(
                impl #impl_generics std::fmt::Debug for #name #type_generics #where_clause {
                    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                        f.debug_struct(#name_str)
                        #fields_token_stream
                    }
                }
            ))
        }
        Err(e) => return Err(e)
    }
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

fn get_fields(derive_input: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }), .. }) = &derive_input.data {
        Ok(named)
    } else {
        Err(syn::Error::new_spanned(derive_input, "Must define struct".to_string()))
    }
}

fn gerenate_fields_tokenstream(fields: &StructFields) -> proc_macro2::TokenStream {
    let mut token_stream = proc_macro2::TokenStream::new();
    fields.iter().for_each(|f| {
        let mut field_desc = FieldDesc::new();
        field_desc.name(&f.ident.as_ref().unwrap());
        if let Some(format) = get_field_attr(&f, "debug") {
            field_desc.format(format);
        }
        token_stream.extend(field_desc.generate_token_stream());
    });
    token_stream.extend(
        quote::quote!(
            .finish()
        )
    );
    token_stream
}

fn get_field_attr<'a>(field: &syn::Field, attr_name: &str) -> Option<String> {
    for attr in &field.attrs {
        if let syn::Meta::NameValue(syn::MetaNameValue { ref path, ref lit, .. }) = attr.parse_meta().unwrap() {
            // eprintln!("{:#?}", path);
            if path.is_ident(attr_name) {
                if let syn::Lit::Str(ref lit_str) = &lit {
                    return Some(lit_str.value());
                }
            }
        }
    }
    None
}


fn get_generics(derive_input: &syn::DeriveInput) -> syn::Generics {
    let mut field_type = Vec::new();
    let mut field_generics_type_for_phantomdata = Vec::new();
    let generic_associated_types = get_generic_associated_types(&derive_input);
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = &derive_input.data {
        for field in fields {
            if let Some(field_type_str) = get_field_type(field) {
                field_type.push(field_type_str);
            }
            if let Some(phantom_data_generics_type) = get_field_generics_type_for_phantomdata(field) {
                field_generics_type_for_phantomdata.push(phantom_data_generics_type);
            }
        }
    }
    let mut generics = derive_input.generics.clone();
    if let Some(hatch) = get_struct_escape_hatch(derive_input) {
        generics.make_where_clause();
        generics.where_clause.as_mut().unwrap().predicates.push(syn::parse_str(hatch.as_str()).unwrap());
    } else {
        for generics_param in generics.params.iter_mut() {
            if let syn::GenericParam::Type(type_param) = generics_param {
                let type_param_name = type_param.ident.to_string();
                // 如果是PhantomData<T>这样的类型，并且T没有单独一个类型的话，就不需要给T增加Debug功能
                // {
                //    field1:PhantomData<T>
                //    field2:T
                // }
                if field_generics_type_for_phantomdata.contains(&type_param_name) && !field_type.contains(&type_param_name) {
                    continue;
                }
                if generic_associated_types.contains_key(&type_param_name) && !field_type.contains(&type_param_name) {
                    continue;
                }
                type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
            }
        }

        generics.make_where_clause();
        for (_, associated_types) in generic_associated_types {
            for associated_type in associated_types {
                generics.where_clause.as_mut().unwrap().predicates.push(syn::parse_quote!(#associated_type:std::fmt::Debug));
            }
        }
    }
    generics
}

fn get_struct_escape_hatch(st: &syn::DeriveInput) -> Option<String> {
    if let Some(inert_attr) = st.attrs.last() {
        if let Ok(syn::Meta::List(syn::MetaList { nested, .. })) = inert_attr.parse_meta() {
            if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue { path, lit, .. }))) = nested.first() {
                if path.is_ident("bound") {
                    if let syn::Lit::Str(ref lit) = lit {
                        return Some(lit.value());
                    }
                }
            }
        }
    }
    None
}

fn get_field_type(field: &syn::Field) -> Option<String> {
    let ty = &field.ty;
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) = ty {
        if let Some(f) = segments.last() {
            return Some(f.ident.to_string());
        }
    }
    None
}

fn get_field_generics_type_for_phantomdata(field: &syn::Field) -> Option<String> {
    let ty = &field.ty;
    if let syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) = ty {
        if let Some(f) = segments.last() {
            if f.ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }) = &f.arguments {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }))) = args.first() {
                        if let Some(path_segment) = segments.first() {
                            return Some(path_segment.ident.to_string());
                        }
                    }
                }
            }
        }
    }
    None
}

struct FieldDesc<'a> {
    name: Option<&'a syn::Ident>,
    format: Option<String>,
}

impl<'a> FieldDesc<'a> {
    pub fn new() -> Self {
        FieldDesc {
            name: None,
            format: None,
        }
    }

    pub fn name(&mut self, name: &'a syn::Ident) {
        self.name = Some(&name);
    }

    pub fn format(&mut self, format: String) {
        self.format = Some(format);
    }

    pub fn generate_token_stream(&self) -> proc_macro2::TokenStream {
        let name_str = self.name.unwrap().to_string();
        let name = self.name;
        if self.format.is_none() {
            quote::quote!(
                .field(#name_str,&self.#name)
            )
        } else {
            let format = &self.format.as_ref().unwrap();
            quote::quote!(
                .field(#name_str,&format_args!(#format,&self.#name))
            )
        }
    }
}


struct TypePathVisitor {
    generic_type_names: Vec<String>,

    associated_types: HashMap<String, Vec<syn::TypePath>>,
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    fn visit_type_path(&mut self, node: &'ast TypePath) {
        if node.path.segments.len() >= 2 {
            let generic_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_name) {
                self.associated_types.entry(generic_name).or_insert(Vec::new()).push(node.clone());
            }
        }
        syn::visit::visit_type_path(self, node);
    }
}

fn get_generic_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    let origin_generic_param_names = st.generics.params.iter().filter_map(|f| {
        if let syn::GenericParam::Type(ty) = f {
            return Some(ty.ident.to_string());
        }
        None
    }).collect();
    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,
        associated_types: HashMap::new(),
    };
    visitor.visit_derive_input(st);
    visitor.associated_types
}