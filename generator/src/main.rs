use heck::SnakeCase;
use itertools::Itertools;
use lazy_static::lazy_static;
use quote::ToTokens;
use std::{
    collections::HashSet,
    io::{Cursor, Write},
    process::Command,
};
use syn::visit::Visit;

lazy_static! {
    static ref DEFINITIONS: syn_codegen::Definitions = serde_json::from_str(include_str!("../../syn/syn.json")).unwrap();
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    fn punctuated_representation(punctuated: &syn_codegen::Punctuated) -> syn_codegen::Type {
        syn_codegen::Type::Vec(Box::new(syn_codegen::Type::Tuple(vec![
            punctuated.element.as_ref().clone(),
            syn_codegen::Type::Option(Box::new(syn_codegen::Type::Token(punctuated.punct.clone()))),
        ])))
    }

    let mut types_output = Cursor::new(Vec::new());

    fn ocaml_type(r#type: &syn_codegen::Type) -> String {
        match r#type {
            syn_codegen::Type::Syn(syn) => format!("rs_{}", syn.to_snake_case()),
            syn_codegen::Type::Std(std) => match std.as_str() {
                "String" => "string".to_string(),
                "u32" => "rs_u32".to_string(),
                "usize" => "rs_usize".to_string(),
                "bool" => "bool".to_string(),
                _ => unreachable!(),
            },
            syn_codegen::Type::Ext(ext) => match ext.as_str() {
                "TokenStream" => "rs_token_stream".to_string(),
                "Literal" => "rs_literal".to_string(),
                "Ident" => "rs_ident".to_string(),
                "Span" => "rs_span".to_string(),
                _ => unreachable!(),
            },
            syn_codegen::Type::Token(token) => format!("rs_token_{}", token.to_snake_case()),
            syn_codegen::Type::Group(group) => ocaml_type(&syn_codegen::Type::Token(group.clone())),
            syn_codegen::Type::Punctuated(punctuated) => ocaml_type(&punctuated_representation(punctuated)),
            syn_codegen::Type::Option(option) => format!("{} option", ocaml_type(option)),
            syn_codegen::Type::Box(r#box) => ocaml_type(r#box),
            syn_codegen::Type::Vec(vec) => format!("{} list", ocaml_type(vec)),
            syn_codegen::Type::Tuple(tuple) => format!("({})", tuple.iter().map(ocaml_type).intersperse(" * ".to_string()).collect::<String>()),
        }
    }

    // A useless type, needed to being able to write `and` instead of `type` everywhere below
    writeln!(types_output, "type rs_dummy = unit")?;

    // Main (syn_codegen::Type::Syn) types
    for node in &DEFINITIONS.types {
        match &node.data {
            syn_codegen::Data::Private => match node.ident.as_str() {
                "LitByteStr" => {
                    writeln!(types_output, "and rs_lit_byte_str = {{")?;
                    writeln!(types_output, "  rs_value : bytes;")?;
                    writeln!(types_output, "  rs_span : rs_span;")?;
                    writeln!(types_output, "}}")?;
                }
                "LitChar" => {
                    writeln!(types_output, "and rs_lit_char = {{")?;
                    writeln!(types_output, "  rs_value : string;")?;
                    writeln!(types_output, "  rs_span : rs_span;")?;
                    writeln!(types_output, "}}")?;
                }
                "LitByte" => {
                    writeln!(types_output, "and rs_lit_byte = {{")?;
                    writeln!(types_output, "  rs_value : char;")?;
                    writeln!(types_output, "  rs_span : rs_span;")?;
                    writeln!(types_output, "}}")?;
                }
                "LitFloat" => {
                    writeln!(types_output, "and rs_lit_float = {{")?;
                    writeln!(types_output, "  rs_digits : string;")?;
                    writeln!(types_output, "  rs_suffix : string;")?;
                    writeln!(types_output, "  rs_span : rs_span;")?;
                    writeln!(types_output, "}}")?;
                }
                "LitInt" => {
                    writeln!(types_output, "and rs_lit_int = {{")?;
                    writeln!(types_output, "  rs_digits : string;")?;
                    writeln!(types_output, "  rs_suffix : string;")?;
                    writeln!(types_output, "  rs_span : rs_span;")?;
                    writeln!(types_output, "}}")?;
                }
                "LitStr" => {
                    writeln!(types_output, "and rs_lit_str = {{")?;
                    writeln!(types_output, "  rs_value : string;")?;
                    writeln!(types_output, "  rs_span : rs_span;")?;
                    writeln!(types_output, "}}")?;
                }
                _ => unreachable!(),
            },
            syn_codegen::Data::Struct(r#struct) => {
                writeln!(types_output, "and rs_{} = {{", node.ident.to_snake_case())?;
                for (name, r#type) in r#struct {
                    if r#type == &syn_codegen::Type::Syn("Reserved".to_string()) {
                        continue;
                    }
                    writeln!(types_output, "  rs_{} : {};", name, ocaml_type(r#type))?;
                }
                writeln!(types_output, "}}")?;
            }
            syn_codegen::Data::Enum(r#enum) => {
                writeln!(types_output, "and rs_{} =", node.ident.to_snake_case())?;
                for (name, types) in r#enum {
                    write!(types_output, "  | Rs{}", name)?;
                    if !types.is_empty() {
                        write!(
                            types_output,
                            " of {}",
                            types.iter().map(ocaml_type).intersperse(" * ".to_string()).collect::<String>()
                        )?;
                    }
                    writeln!(types_output)?;
                }
            }
        }
    }

    // Tokens, with ones from types::Type::Group added
    for (name, representation) in DEFINITIONS
        .tokens
        .iter()
        .map(|(name, representation)| (name.as_str(), representation.as_str()))
        .chain(vec![("Brace", "{...}"), ("Bracket", "[...]"), ("Paren", "(...)"), ("Group", "None-delimited group")].into_iter())
    {
        writeln!(
            types_output,
            "and {} = rs_span (* {} *)",
            ocaml_type(&syn_codegen::Type::Token(name.to_string())),
            representation
        )?;
    }

    // Types for syn_codegen::Type::Std
    writeln!(types_output, "and rs_u32 = int (* Should be enough for where it is used *)")?;
    writeln!(types_output, "and rs_usize = int (* Should be enough for where it is used *)")?;

    // Types for syn_codegen::Type::Ext
    writeln!(types_output, "and rs_delimiter = ")?;
    writeln!(types_output, "  | RsParenthesis")?;
    writeln!(types_output, "  | RsBrace")?;
    writeln!(types_output, "  | RsBracket")?;
    writeln!(types_output, "  | RsNone")?;
    writeln!(types_output, "and rs_spacing = ")?;
    writeln!(types_output, "  | RsAlone")?;
    writeln!(types_output, "  | RsJoint")?;
    writeln!(types_output, "and rs_line_column = {{")?;
    writeln!(types_output, "  rs_line : int;")?;
    writeln!(types_output, "  rs_column : int;")?;
    writeln!(types_output, "}}")?;
    writeln!(types_output, "and rs_span = {{")?;
    writeln!(types_output, "  rs_source_file : string;")?;
    writeln!(types_output, "  rs_start : rs_line_column;")?;
    writeln!(types_output, "  rs_end : rs_line_column;")?;
    writeln!(types_output, "}}")?;
    writeln!(types_output, "and rs_group = {{")?;
    writeln!(types_output, "  rs_delimiter : rs_delimiter;")?;
    writeln!(types_output, "  rs_stream : rs_token_stream;")?;
    writeln!(types_output, "  rs_span : rs_span;")?;
    writeln!(types_output, "}}")?;
    writeln!(types_output, "and rs_ident = {{")?;
    writeln!(types_output, "  rs_string : string;")?;
    writeln!(types_output, "  rs_span : rs_span;")?;
    writeln!(types_output, "}}")?;
    writeln!(types_output, "and rs_punct = {{")?;
    writeln!(types_output, "  rs_char : string;")?;
    writeln!(types_output, "  rs_spacing : rs_spacing;")?;
    writeln!(types_output, "  rs_span : rs_span;")?;
    writeln!(types_output, "}}")?;
    writeln!(types_output, "and rs_literal = {{")?;
    writeln!(types_output, "  rs_text : string;")?;
    writeln!(types_output, "  rs_span : rs_span;")?;
    writeln!(types_output, "}}")?;
    writeln!(types_output, "and rs_token_tree = ")?;
    writeln!(types_output, "  | RsGroup of rs_group")?;
    writeln!(types_output, "  | RsIdent of rs_ident")?;
    writeln!(types_output, "  | RsPunct of rs_punct")?;
    writeln!(types_output, "  | RsLiteral of rs_literal")?;
    writeln!(types_output, "and rs_token_stream = rs_token_tree list")?;

    let types_output = String::from_utf8(types_output.into_inner())?;

    let mut rust_file = std::fs::File::create("../rust-lib/src/generated.rs")?;
    writeln!(rust_file, "/* Automatically generated */")?;
    writeln!(rust_file)?;
    writeln!(rust_file, "use quote::ToTokens;")?;

    let command_output = Command::new("cargo")
        .current_dir("../syn")
        .arg("expand")
        .arg("--ugly")
        .arg("--features")
        .arg("full")
        .output()?;
    if !command_output.status.success() {
        std::io::stderr().write_all(&command_output.stderr)?;
        return Err("Unable to run `cargo expand --features full`".into());
    }

    struct Visitor {
        parsable_types: HashSet<String>,
        generatable_types: HashSet<String>,
        non_exhaustive_types: HashSet<String>,
    }

    impl<'ast> syn::visit::Visit<'ast> for Visitor {
        fn visit_item_enum(&mut self, node: &'ast syn::ItemEnum) {
            for variant in &node.variants {
                if variant.ident == "__TestExhaustive" {
                    self.non_exhaustive_types.insert(node.ident.to_string());
                }
            }
            syn::visit::visit_item_enum(self, node);
        }

        fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
            if let syn::Type::Path(syn::TypePath { path: self_ty_path, .. }) = node.self_ty.as_ref() {
                if let Some(self_ty_ident) = self_ty_path.get_ident() {
                    if let Some((_, trait_path, _)) = &node.trait_ {
                        match trait_path.to_token_stream().to_string().as_str() {
                            "Parse" => {
                                self.parsable_types.insert(self_ty_ident.to_string());
                            }
                            "ToTokens" | ":: quote :: ToTokens" => {
                                self.generatable_types.insert(self_ty_ident.to_string());
                            }
                            _ => {}
                        }
                    }
                }
            }
            syn::visit::visit_item_impl(self, node);
        }
    }

    let mut visitor = Visitor {
        parsable_types: HashSet::new(),
        generatable_types: HashSet::new(),
        non_exhaustive_types: HashSet::new(),
    };

    visitor.visit_file(&syn::parse_str::<syn::File>(&String::from_utf8(command_output.stdout)?)?);

    let Visitor {
        parsable_types,
        generatable_types,
        non_exhaustive_types,
    } = visitor;

    eprintln!("Non-exhaustive types: {:?}", non_exhaustive_types);

    fn marshal_output_type(r#type: &syn_codegen::Type, input: &str) -> String {
        match r#type {
            syn_codegen::Type::Syn(syn) => format!("marshal_output_{}(marshaling_output, {})", syn.to_snake_case(), input),
            syn_codegen::Type::Std(std) => match std.as_str() {
                "String" => format!("marshal_output_string(marshaling_output, {})", input),
                "u32" => format!("marshal_output_u32(marshaling_output, {})", input),
                "usize" => format!("marshal_output_usize(marshaling_output, {})", input),
                "bool" => format!("marshal_output_bool(marshaling_output, {})", input),
                _ => unreachable!(),
            },
            syn_codegen::Type::Ext(ext) => match ext.as_str() {
                "TokenStream" => format!("marshal_output_token_stream(marshaling_output, {})", input),
                "Literal" => format!("marshal_output_literal(marshaling_output, {})", input),
                "Ident" => format!("marshal_output_ident(marshaling_output, {})", input),
                "Span" => format!("marshal_output_span(marshaling_output, {})", input),
                _ => unreachable!(),
            },
            syn_codegen::Type::Token(token) => {
                if DEFINITIONS.tokens.get(token).unwrap().chars().next().unwrap().is_ascii_alphabetic() {
                    marshal_output_type(
                        &syn_codegen::Type::Ext("Span".to_string()),
                        &format!("{}.span", input)
                    )
                } else {
                    marshal_output_type(
                        &syn_codegen::Type::Ext("Span".to_string()),
                        &format!("{}.spans.iter().copied().reduce(|a, b| a.join(b).unwrap()).unwrap()", input)
                    )
                }
            }
            syn_codegen::Type::Group(_) => marshal_output_type(&syn_codegen::Type::Ext("Span".to_string()), &format!("{}.span", input)),
            syn_codegen::Type::Punctuated(punctuated) => marshal_output_type(
                &punctuated_representation(punctuated),
                &format!("{}.into_pairs().map(|pair| match pair {{ syn::punctuated::Pair::Punctuated(t, p) => (t, Some(p)), syn::punctuated::Pair::End(t) => (t, None) }}).collect()", input),
            ),
            syn_codegen::Type::Option(option) => format!(
                "marshal_output_option(marshaling_output, {}, |marshaling_output, x| {})",
                input,
                marshal_output_type(option, "x")
            ),
            syn_codegen::Type::Box(r#box) => marshal_output_type(r#box, &format!("{}.as_ref().clone()", input)),
            syn_codegen::Type::Vec(vec) => format!(
                "marshal_output_list(marshaling_output, {}, |marshaling_output, x| {})",
                input,
                marshal_output_type(vec, "x")
            ),
            syn_codegen::Type::Tuple(tuple) => format!(
                "{{ let input = {0}; marshaling_output.add_object({1}, {1}); marshaling_output.write_header({1}, 0); {2} }}",
                input,
                tuple.len(),
                tuple.iter()
                    .enumerate()
                    .map(|(i, t)| marshal_output_type(t, &format!("input.{}", i)))
                    .intersperse("; ".to_string())
                    .collect::<String>()
            ),
        }
    }

    fn marshal_input_type(r#type: &syn_codegen::Type) -> String {
        match r#type {
            syn_codegen::Type::Syn(syn) => {
                format!("marshal_input_{}(marshaling_input)", syn.to_snake_case())
            }
            syn_codegen::Type::Std(std) => match std.as_str() {
                "String" => "marshal_input_string(marshaling_input)".to_string(),
                "u32" => "marshal_input_u32(marshaling_input)".to_string(),
                "usize" => "marshal_input_usize(marshaling_input)".to_string(),
                "bool" => "marshal_input_bool(marshaling_input)".to_string(),
                _ => unreachable!(),
            },
            syn_codegen::Type::Ext(ext) => match ext.as_str() {
                "TokenStream" => "marshal_input_token_stream(marshaling_input)".to_string(),
                "Literal" => "marshal_input_literal(marshaling_input)".to_string(),
                "Ident" => "marshal_input_ident(marshaling_input)".to_string(),
                "Span" => "marshal_input_span(marshaling_input)".to_string(),
                _ => unreachable!(),
            },
            syn_codegen::Type::Token(token) => {
                if DEFINITIONS.tokens.get(token).unwrap().chars().next().unwrap().is_ascii_alphabetic() {
                    format!(
                        "syn::token::{} {{ span: {} }}",
                        token,
                        marshal_input_type(&syn_codegen::Type::Ext("Span".to_string()))
                    )
                } else {
                    format!(
                        "syn::token::{} {{ spans: [{}; {}] }}",
                        token,
                        marshal_input_type(&syn_codegen::Type::Ext("Span".to_string())),
                        DEFINITIONS.tokens.get(token).unwrap().len()
                    )
                }
            }
            syn_codegen::Type::Group(group) => format!(
                "syn::token::{} {{ span: {} }}",
                group,
                marshal_input_type(&syn_codegen::Type::Ext("Span".to_string()))
            ),
            syn_codegen::Type::Punctuated(punctuated) => format!(
                "{}.into_iter().map(|(t, p)| if let Some(p) = p {{ syn::punctuated::Pair::Punctuated(t, p) }} else {{ syn::punctuated::Pair::End(t) }}).collect()",
                marshal_input_type(&punctuated_representation(punctuated))
            ),
            syn_codegen::Type::Option(option) => format!(
                "marshal_input_option(marshaling_input, |marshaling_input| {})",
                marshal_input_type(option)
            ),
            syn_codegen::Type::Box(r#box) => format!("Box::new({})", marshal_input_type(r#box)),
            syn_codegen::Type::Vec(vec) => format!("marshal_input_list(marshaling_input, |marshaling_input| {})", marshal_input_type(vec)),
            syn_codegen::Type::Tuple(tuple) => format!(
                "{{ assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0))); ({}) }}",
                tuple.iter()
                    .map(|t| marshal_input_type(t))
                    .intersperse(", ".to_string())
                    .collect::<String>()
            ),
        }
    }

    for node in &DEFINITIONS.types {
        match &node.data {
            syn_codegen::Data::Private => {}
            syn_codegen::Data::Struct(r#struct) => {
                writeln!(rust_file)?;
                writeln!(
                    rust_file,
                    "fn marshal_output_{}(marshaling_output: &mut MarshalingOutput, value: syn::{}) {{",
                    node.ident.to_snake_case(),
                    node.ident
                )?;
                writeln!(rust_file, "    marshaling_output.add_object({0}, {0});", r#struct.len())?;
                writeln!(rust_file, "    marshaling_output.write_header({}, 0);", r#struct.len())?;
                for (name, r#type) in r#struct {
                    if r#type == &syn_codegen::Type::Syn("Reserved".to_string()) {
                        continue;
                    }
                    writeln!(rust_file, "    {};", marshal_output_type(r#type, &format!("value.{}", name)))?;
                }
                writeln!(rust_file, "}}")?;
                writeln!(rust_file)?;
                writeln!(
                    rust_file,
                    "fn marshal_input_{}(marshaling_input: &mut MarshalingInput) -> syn::{} {{",
                    node.ident.to_snake_case(),
                    node.ident
                )?;
                writeln!(rust_file, "    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));")?;
                writeln!(rust_file, "    syn::{} {{", node.ident)?;
                let mut seen_reserved = false;
                for (name, r#type) in r#struct {
                    if r#type == &syn_codegen::Type::Syn("Reserved".to_string()) {
                        seen_reserved = true;
                        continue;
                    }
                    writeln!(rust_file, "        {}: {},", name, marshal_input_type(r#type))?;
                }
                if seen_reserved {
                    match node.ident.as_str() {
                        "ExprReference" => writeln!(rust_file, "        ..syn::parse_str::<syn::ExprReference>(\"&a\").unwrap()")?,
                        _ => unreachable!(),
                    }
                }
                writeln!(rust_file, "    }}")?;
                writeln!(rust_file, "}}")?;
            }
            syn_codegen::Data::Enum(r#enum) => {
                writeln!(rust_file)?;
                writeln!(
                    rust_file,
                    "fn marshal_output_{}(marshaling_output: &mut MarshalingOutput, value: syn::{}) {{",
                    node.ident.to_snake_case(),
                    node.ident
                )?;
                writeln!(rust_file, "    match value {{")?;
                let mut without_fields_count = 0;
                let mut with_fields_count = 0;
                for (name, types) in r#enum {
                    write!(rust_file, "        syn::{}::{}", node.ident, name)?;
                    if !types.is_empty() {
                        write!(
                            rust_file,
                            "({})",
                            types
                                .iter()
                                .enumerate()
                                .map(|(i, _)| format!("v{}", i))
                                .intersperse(", ".to_string())
                                .collect::<String>()
                        )?;
                    }
                    writeln!(rust_file, " => {{")?;
                    if types.is_empty() {
                        writeln!(rust_file, "            marshaling_output.write_int({});", without_fields_count)?;
                        without_fields_count += 1;
                    } else {
                        writeln!(rust_file, "            marshaling_output.add_object({0}, {0});", types.len())?;
                        writeln!(rust_file, "            marshaling_output.write_header({}, {});", types.len(), with_fields_count)?;
                        for (i, r#type) in types.iter().enumerate() {
                            writeln!(rust_file, "            {};", marshal_output_type(r#type, &format!("v{}", i)))?;
                        }
                        with_fields_count += 1;
                    }
                    writeln!(rust_file, "        }}")?;
                }
                if non_exhaustive_types.contains(&node.ident) {
                    writeln!(rust_file, "        syn::{}::__TestExhaustive(_) => unreachable!()", node.ident)?;
                }
                writeln!(rust_file, "    }}")?;
                writeln!(rust_file, "}}")?;
                writeln!(rust_file)?;
                writeln!(
                    rust_file,
                    "fn marshal_input_{}(marshaling_input: &mut MarshalingInput) -> syn::{} {{",
                    node.ident.to_snake_case(),
                    node.ident
                )?;
                writeln!(rust_file, "    match marshaling_input.read_int_or_block() {{")?;
                let mut without_fields_count = 0;
                let mut with_fields_count = 0;
                for (name, types) in r#enum {
                    if types.is_empty() {
                        writeln!(rust_file, "        IntOrBlock::Int({}) => syn::{}::{},", without_fields_count, node.ident, name)?;
                        without_fields_count += 1;
                    } else {
                        writeln!(rust_file, "        IntOrBlock::Block({}) => syn::{}::{}(", with_fields_count, node.ident, name)?;
                        for r#type in types {
                            writeln!(rust_file, "            {},", marshal_input_type(r#type))?;
                        }
                        writeln!(rust_file, "        ),")?;
                        with_fields_count += 1;
                    }
                }
                writeln!(rust_file, "        _ => panic!(\"Unknown tag for {}\"),", node.ident)?;
                writeln!(rust_file, "    }}")?;
                writeln!(rust_file, "}}")?;
            }
        }
    }

    let mut mli_file = std::fs::File::create("../ocaml-lib/ocaml_rust_parser_generator.mli")?;
    writeln!(mli_file, "(* Automatically generated *)")?;
    writeln!(mli_file)?;
    write!(mli_file, "{}", &types_output)?;

    let mut ml_file = std::fs::File::create("../ocaml-lib/ocaml_rust_parser_generator.ml")?;
    writeln!(ml_file, "(* Automatically generated *)")?;
    writeln!(ml_file)?;
    write!(ml_file, "{}", &types_output)?;

    writeln!(mli_file)?;
    writeln!(mli_file, "val token_stream_of_string : string -> rs_token_stream")?;
    writeln!(mli_file, "val string_of_token_stream : rs_token_stream -> string")?;

    writeln!(ml_file)?;
    writeln!(
        ml_file,
        "external native_token_stream_of_string : string -> bytes = \"native_token_stream_of_string\""
    )?;
    writeln!(
        ml_file,
        "external native_string_of_token_stream : bytes -> string = \"native_string_of_token_stream\""
    )?;

    writeln!(ml_file)?;
    writeln!(ml_file, "let token_stream_of_string x = Marshal.from_bytes (native_token_stream_of_string x) 0")?;
    writeln!(
        ml_file,
        "let string_of_token_stream x = native_string_of_token_stream (Marshal.to_bytes x [Marshal.No_sharing])"
    )?;

    for node in &DEFINITIONS.types {
        let type_name = &node.ident;
        let parsable = parsable_types.contains(type_name);
        let generatable = generatable_types.contains(type_name);
        if !parsable {
            eprintln!("No Parse implementation for {} found", type_name);
        }
        if !generatable {
            eprintln!("No ToTokens implementation for {} found", type_name);
        }
        if !parsable && !generatable {
            continue;
        }

        writeln!(mli_file)?;
        if parsable {
            writeln!(
                mli_file,
                "val parse_{0}_from_token_stream : rs_token_stream -> rs_{0}",
                type_name.to_snake_case()
            )?;
            writeln!(mli_file, "val parse_{0}_from_string : string -> rs_{0}", type_name.to_snake_case())?;
        }
        if generatable {
            writeln!(
                mli_file,
                "val generate_{0}_to_token_stream : rs_{0} -> rs_token_stream",
                type_name.to_snake_case()
            )?;
            writeln!(mli_file, "val generate_{0}_to_string : rs_{0} -> string", type_name.to_snake_case())?;
        }

        writeln!(ml_file)?;
        if parsable {
            writeln!(
                ml_file,
                "external native_parse_{0}_from_token_stream : bytes -> bytes = \"native_parse_{0}_from_token_stream\"",
                type_name.to_snake_case()
            )?;
            writeln!(
                ml_file,
                "external native_parse_{0}_from_string : string -> bytes = \"native_parse_{0}_from_string\"",
                type_name.to_snake_case()
            )?;
        }
        if generatable {
            writeln!(
                ml_file,
                "external native_generate_{0}_to_token_stream : bytes -> bytes = \"native_generate_{0}_to_token_stream\"",
                type_name.to_snake_case()
            )?;
            writeln!(
                ml_file,
                "external native_generate_{0}_to_string : bytes -> string = \"native_generate_{0}_to_string\"",
                type_name.to_snake_case()
            )?;
        }

        writeln!(ml_file)?;
        if parsable {
            writeln!(
                ml_file,
                "let parse_{0}_from_token_stream x = Marshal.from_bytes (native_parse_{0}_from_token_stream (Marshal.to_bytes x [Marshal.No_sharing])) 0",
                type_name.to_snake_case()
            )?;
            writeln!(
                ml_file,
                "let parse_{0}_from_string x = Marshal.from_bytes (native_parse_{0}_from_string x) 0",
                type_name.to_snake_case()
            )?;
        }
        if generatable {
            writeln!(
                ml_file,
                "let generate_{0}_to_token_stream x = Marshal.from_bytes (native_generate_{0}_to_token_stream (Marshal.to_bytes x [Marshal.No_sharing])) 0",
                type_name.to_snake_case()
            )?;
            writeln!(
                ml_file,
                "let generate_{0}_to_string x = native_generate_{0}_to_string (Marshal.to_bytes x [Marshal.No_sharing])",
                type_name.to_snake_case()
            )?;
        }

        if parsable {
            writeln!(rust_file)?;
            writeln!(rust_file, "#[ocaml::func]")?;
            writeln!(
                rust_file,
                "pub fn native_parse_{}_from_token_stream(x: &[u8]) -> ocaml::Value {{",
                type_name.to_snake_case()
            )?;
            writeln!(rust_file, "    let mut marshaling_input = MarshalingInput::new(x.into());")?;
            writeln!(rust_file, "    let mut marshaling_output = MarshalingOutput::new();")?;
            writeln!(
                rust_file,
                "    marshal_output_{}(&mut marshaling_output, syn::parse2::<syn::{}>(marshal_input_token_stream(&mut marshaling_input)).unwrap());",
                type_name.to_snake_case(),
                type_name
            )?;
            writeln!(rust_file, "    unsafe {{ ocaml::Value::bytes(&marshaling_output.finish()) }}")?;
            writeln!(rust_file, "}}")?;
            writeln!(rust_file)?;
            writeln!(rust_file, "#[ocaml::func]")?;
            writeln!(
                rust_file,
                "pub fn native_parse_{}_from_string(x: &str) -> ocaml::Value {{",
                type_name.to_snake_case()
            )?;
            writeln!(rust_file, "    let mut marshaling_output = MarshalingOutput::new();")?;
            writeln!(
                rust_file,
                "    marshal_output_{}(&mut marshaling_output, syn::parse_str::<syn::{}>(x).unwrap());",
                type_name.to_snake_case(),
                type_name
            )?;
            writeln!(rust_file, "    unsafe {{ ocaml::Value::bytes(&marshaling_output.finish()) }}")?;
            writeln!(rust_file, "}}")?;
        }
        if generatable {
            writeln!(rust_file)?;
            writeln!(rust_file, "#[ocaml::func]")?;
            writeln!(
                rust_file,
                "pub fn native_generate_{}_to_token_stream(x: &[u8]) -> ocaml::Value {{",
                type_name.to_snake_case()
            )?;
            writeln!(rust_file, "    let mut marshaling_input = MarshalingInput::new(x.into());")?;
            writeln!(rust_file, "    let mut marshaling_output = MarshalingOutput::new();")?;
            writeln!(
                rust_file,
                "    marshal_output_token_stream(&mut marshaling_output, marshal_input_{}(&mut marshaling_input).to_token_stream());",
                type_name.to_snake_case()
            )?;
            writeln!(rust_file, "    unsafe {{ ocaml::Value::bytes(&marshaling_output.finish()) }}")?;
            writeln!(rust_file, "}}")?;
            writeln!(rust_file)?;
            writeln!(rust_file, "#[ocaml::func]")?;
            writeln!(
                rust_file,
                "pub fn native_generate_{}_to_string(x: &[u8]) -> String {{",
                type_name.to_snake_case()
            )?;
            writeln!(rust_file, "    let mut marshaling_input = MarshalingInput::new(x.into());")?;
            writeln!(
                rust_file,
                "    marshal_input_{}(&mut marshaling_input).to_token_stream().to_string()",
                type_name.to_snake_case()
            )?;
            writeln!(rust_file, "}}")?;
        }
    }

    Ok(())
}
