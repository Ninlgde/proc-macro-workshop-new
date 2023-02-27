use proc_macro::TokenStream;
use quote::quote;
use syn;
use syn::spanned::Spanned;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// alias of `struct`'s `fields`
type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;

/// 宏扩展方法
fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    let struct_ident = &st.ident;

    // 从input中获取struct的fields
    let fields = get_fields_from_derive_input(st)?;
    // 根据struct的fields生成builder的fields
    let builder_struct_fields_def = generate_builder_struct_fields_def(fields)?;
    // builder的初始化方法中fields初始化列表
    let builder_struct_factory_init_clauses = generate_builder_struct_factory_init_clauses(fields)?;
    // 生成好的各种setter方法
    let setter_functions = generate_setter_functions(fields)?;
    // 生成好的build方法
    let build_function = generate_build_function(fields, struct_ident)?;

    let ret = quote! {
        // 构造 pub struct xxxBuilder {}
        pub struct #builder_name_ident {
            // 在当前这个`quote!`宏中，引用了其他`quote!`宏返回的结果
            // 在这里把不同的代码碎片拼接起来，就像搭积木一样
            #builder_struct_fields_def
        }

        // 给struct添加builder方法
        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    // 重复展开了每一个字段
                    #(#builder_struct_factory_init_clauses),*
                }
            }
        }

        // 给builder添加setter和build方法
        impl #builder_name_ident {
            #setter_functions
            #build_function
        }
    };
    return Ok(ret);
}

/// 从DeriveInput中获取struct的fields
fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    // 根据语法树找到需要的named
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = d.data
    {
        return Ok(named);
    }
    // 没找到报错
    Err(syn::Error::new_spanned(
        d,
        "Must define on a Struct, not Enum".to_string(),
    ))
}

/// 根据type和外部的name获取相应的type
fn get_generic_inner_type<'a>(ty: &'a syn::Type, outer_ident_name: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        // 这里我们取segments的最后一节来判断是不是`outer_ident_name<T>`
        if let Some(seg) = path.segments.last() {
            if seg.ident == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.first() {
                        return Some(inner_ty);
                    }
                }
            }
        }
    }
    None
}

/// vec的field 获取 #[builder(each = "xxx")] 这个标记
fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::List(syn::MetaList {
            ref path,
            ref nested,
            ..
        })) = attr.parse_meta()
        {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Ok(Some(syn::Ident::new(
                                    ident_str.value().as_str(),
                                    attr.span(),
                                )));
                            }
                        } else {
                            // 注意这里new_spanned函数的参数，我们需要在语法树中找到一个合适的节点来获取它的span，如果这个语法树节点找的不对，产生出的错误信息就会不一样
                            if let Ok(syn::Meta::List(ref list)) = attr.parse_meta() {
                                return Err(syn::Error::new_spanned(
                                    list,
                                    r#"expected `builder(each = "...")`"#,
                                ));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

/// 根据struct的fields生成builder的fields
fn generate_builder_struct_fields_def(
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: syn::Result<Vec<proc_macro2::TokenStream>> = fields
        .iter()
        .map(|f| {
            // 针对是否为`Option`类型字段，产生不同的结果
            if let Some(inner_ty) = get_generic_inner_type(&f.ty, "Option") {
                Ok(quote!(std::option::Option<#inner_ty>))
            } else if get_user_specified_ident_for_vec(f)?.is_some() {
                let origin_ty = &f.ty;
                Ok(quote!(#origin_ty)) // 如果用户指定了each属性，我们就可以认为它一定是作用在一个Vec字段上
            } else {
                let origin_ty = &f.ty;
                Ok(quote!(std::option::Option<#origin_ty>))
            }
        })
        .collect();

    let types = types?;
    let token_stream = quote! {
        #(#idents: #types),*
    };
    Ok(token_stream)
}

/// builder的初始化方法中fields初始化列表
fn generate_builder_struct_factory_init_clauses(
    fields: &StructFields,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: syn::Result<Vec<proc_macro2::TokenStream>> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            if get_user_specified_ident_for_vec(f)?.is_some() {
                Ok(quote! {
                    #ident: std::vec::Vec::new()  //指定了each属性的Vec需要初始化
                })
            } else {
                Ok(quote! {
                    #ident: std::option::Option::None
                })
            }
        })
        .collect();

    Ok(init_clauses?)
}

/// 生成setter方法
fn generate_setter_functions(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut setters = proc_macro2::TokenStream::new();
    for (idx, (ident, ty)) in idents.iter().zip(types.iter()).enumerate() {
        let mut setter;
        if let Some(inner_ty) = get_generic_inner_type(ty, "Option") {
            // Option的field, 使用 std::option::Option::Some()包裹inner_ty
            setter = quote! {
                fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        } else if let Some(ref user_specified_ident) =
            get_user_specified_ident_for_vec(&fields[idx])?
        {
            // 根据 get_user_specified_ident_for_vec 获取的type, 生成相应的单个添加方法
            let inner_ty = get_generic_inner_type(ty, "Vec").ok_or(syn::Error::new(
                fields[idx].span(),
                "each field must be specified with Vec field",
            ))?;
            setter = quote! {
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_ty) -> &mut Self {
                    self.#ident.push(#user_specified_ident);
                    self
                }
            };
            // 如果用户指定的setter名字和原始字段的名字不一样，那么产生另一个setter，这个setter是一次性传入一个列表的
            if user_specified_ident != ident.as_ref().unwrap() {
                setter.extend(quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = #ident.clone();
                        self
                    }
                });
            }
        } else {
            // 其他情况, 使用 std::option::Option::Some()包裹ty
            setter = quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            };
        }

        setters.extend(setter);
    }

    Ok(setters)
}

/// 生成build方法
fn generate_build_function(
    fields: &StructFields,
    origin_struct_ident: &syn::Ident,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    // 生成检查方法
    let mut checker_code_pieces = vec![];
    for (idx, (ident, ty)) in idents.iter().zip(types.iter()).enumerate() {
        if get_generic_inner_type(ty, "Option").is_none()
            && get_user_specified_ident_for_vec(&fields[idx])?.is_none()
        {
            checker_code_pieces.push(quote! {
                if self.#ident.is_none() {
                    let err = format!("{} field missing", stringify!(#ident));
                    return std::result::Result::Err(err.into())
                }
            })
        }
    }

    // 生成builder填充struct的各种语句
    let mut fill_result_clauses = vec![];
    for (idx, (ident, ty)) in idents.iter().zip(types.iter()).enumerate() {
        if get_user_specified_ident_for_vec(&fields[idx])?.is_some() {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone()
            });
        } else if get_generic_inner_type(ty, "Option").is_none() {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone().unwrap()
            })
        } else {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone()
            })
        }
    }

    // 生成最终的build方法
    let ts = quote! {
        pub fn build(&mut self) -> std::result::Result<#origin_struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#checker_code_pieces)*

            let ret = #origin_struct_ident {
                #(#fill_result_clauses),*
            };
            std::result::Result::Ok(ret)
        }
    };

    Ok(ts)
}
