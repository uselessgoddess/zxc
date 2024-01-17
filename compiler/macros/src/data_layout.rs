use {proc_macro2::TokenStream, quote::quote, syn::LitStr};

use abi::{AbiPrefAlign, Align, Endian, Size, TargetDataLayout};

pub fn imp(input: TokenStream) -> TokenStream {
    let (input, span) = match syn::parse2::<LitStr>(input) {
        Ok(data) => (data.value(), data.span()),
        Err(err) => {
            return err.to_compile_error();
        }
    };

    let dl = match TargetDataLayout::parse_from_llvm_datalayout_string(&input) {
        Ok(layout) => layout,
        Err(err) => return syn::Error::new(span, format!("{err:?}")).to_compile_error(),
    };
    //abi::TargetDataLayout {
    //    endian: Endian::Little,
    //    i1_align: AbiPrefAlign {},
    //    i8_align: AbiPrefAlign {},
    //    i16_align: AbiPrefAlign {},
    //    i32_align: AbiPrefAlign {},
    //    i64_align: AbiPrefAlign {},
    //    i128_align: AbiPrefAlign {},
    //    f32_align: AbiPrefAlign {},
    //    f64_align: AbiPrefAlign {},
    //    pointer_size: (),
    //    pointer_align: AbiPrefAlign {},
    //    aggregate_align: AbiPrefAlign {},
    //    vector_align: vec![],
    //    instruction_address_space: AddressSpace(),
    //};

    let prefix = quote!(crate::abi);
    let endian = match dl.endian {
        Endian::Little => quote!(#prefix::Endian::Little),
        Endian::Big => quote!(#prefix::Endian::Big),
    };
    let quote_align = |align: Align| {
        let pow = align.pow2();
        quote!(unsafe { #prefix::Align::from_pow2(#pow) })
    };
    let quote_size = |size: Size| {
        let raw = size.bytes();
        quote! { #prefix::Size::from_bytes(#raw) }
    };
    let quote_abi = |align: AbiPrefAlign| {
        let abi = quote_align(align.abi);
        let pref = quote_align(align.pref);
        quote!(#prefix::AbiPrefAlign {
            abi: #abi,
            pref: #pref,
        })
    };

    let i1 = quote_abi(dl.i1_align);
    let i8 = quote_abi(dl.i8_align);
    let i16 = quote_abi(dl.i16_align);
    let i32 = quote_abi(dl.i32_align);
    let i64 = quote_abi(dl.i64_align);
    let i128 = quote_abi(dl.i128_align);
    let ptr = quote_abi(dl.pointer_align);
    let f32 = quote_abi(dl.f32_align);
    let f64 = quote_abi(dl.f64_align);
    let aggregate = quote_abi(dl.aggregate_align);
    let ptr_size = quote_size(dl.pointer_size);

    let mut buf = quote! {};
    for (size, align) in dl.vector_align {
        let size = quote_size(size);
        let align = quote_abi(align);
        buf = quote! {
            #buf
            (#size, #align),
        };
    }

    let space = dl.instruction_address_space.0;
    quote! {
        #prefix::TargetDataLayout {
            endian: #endian,
            i1_align: #i1,
            i8_align: #i8,
            i16_align: #i16,
            i32_align: #i32,
            i64_align: #i64,
            i128_align: #i128,
            pointer_align: #ptr,
            f32_align: #f32,
            f64_align: #f64,
            aggregate_align: #aggregate,
            pointer_size: #ptr_size,
            vector_align: vec![#buf],
            instruction_address_space: #prefix::AddressSpace(#space),
        }
    }
}
