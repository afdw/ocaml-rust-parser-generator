use byteorder::{BigEndian, LittleEndian, ReadBytesExt, WriteBytesExt};
use std::{
    io::{Cursor, Read, Write},
    str::FromStr,
};
use syn::spanned::Spanned;

const MAGIC_NUMBER_SMALL: u32 = 0x8495A6BE;
const MAGIC_NUMBER_BIG: u32 = 0x8495A6BF;

struct MarshalingOutput {
    data: Cursor<Vec<u8>>,
    size_32: u64,
    size_64: u64,
}

impl MarshalingOutput {
    fn new() -> Self {
        Self {
            data: Cursor::new(Vec::new()),
            size_32: 0,
            size_64: 0,
        }
    }

    fn add_object(&mut self, size_32: u64, size_64: u64) {
        self.size_32 += 1 + size_32;
        self.size_64 += 1 + size_64;
    }

    fn write_int(&mut self, n: u64) {
        if n as i64 >= 0 && (n as i64) < 0x40 {
            self.data.write_u8(0x40 + n as u8).unwrap();
        } else if n as i64 >= -(1 << 7) && n as i64 <= 1 << 7 {
            self.data.write_u8(0x00).unwrap();
            self.data.write_u8(n as u8).unwrap();
        } else if n as i64 >= -(1 << 15) && n as i64 <= 1 << 15 {
            self.data.write_u8(0x01).unwrap();
            self.data.write_u16::<BigEndian>(n as u16).unwrap();
        } else if n as i64 >= -(1 << 30) && n as i64 <= 1 << 30 {
            self.data.write_u8(0x02).unwrap();
            self.data.write_u32::<BigEndian>(n as u32).unwrap();
        } else {
            self.data.write_u8(0x03).unwrap();
            self.data.write_u64::<BigEndian>(n).unwrap();
        }
    }

    fn write_header(&mut self, size: u64, tag: u8) {
        if size < 8 && tag < 16 {
            self.data.write_u8(0x80 + tag + ((size as u8) << 4)).unwrap();
        } else {
            assert!(size < 1 << 54);
            let header: u64 = ((size as u64) << 10) | tag as u64;
            if header < 1 << 32 {
                self.data.write_u8(0x08).unwrap();
                self.data.write_u32::<BigEndian>(header as u32).unwrap();
            } else {
                self.data.write_u8(0x13).unwrap();
                self.data.write_u64::<BigEndian>(header).unwrap();
            }
        }
    }

    fn write_double(&mut self, v: f64) {
        self.data.write_u8(0x0C).unwrap();
        self.data.write_f64::<LittleEndian>(v).unwrap();
    }

    fn write_string(&mut self, s: &[u8]) {
        if (s.len() as u64) < 0x20 {
            self.data.write_u8(0x20 + s.len() as u8).unwrap();
        } else if (s.len() as u64) < 1 << 8 {
            self.data.write_u8(0x09).unwrap();
            self.data.write_u8(s.len() as u8).unwrap();
        } else if (s.len() as u64) < 1 << 32 {
            self.data.write_u8(0x0A).unwrap();
            self.data.write_u32::<BigEndian>(s.len() as u32).unwrap();
        } else {
            self.data.write_u8(0x15).unwrap();
            self.data.write_u64::<BigEndian>(s.len() as u64).unwrap();
        }
        self.data.write_all(s).unwrap();
    }

    fn finish(mut self) -> Vec<u8> {
        let result_length = self.data.position();
        if result_length < 1 << 32 && self.size_32 < 1 << 32 && self.size_64 < 1 << 32 {
            self.data.write_u32::<BigEndian>(MAGIC_NUMBER_SMALL).unwrap();
            self.data.write_u32::<BigEndian>(result_length as u32).unwrap();
            self.data.write_u32::<BigEndian>(0).unwrap();
            self.data.write_u32::<BigEndian>(self.size_32 as u32).unwrap();
            self.data.write_u32::<BigEndian>(self.size_64 as u32).unwrap();
            let mut result = self.data.into_inner();
            result.rotate_right(20);
            result
        } else {
            self.data.write_u32::<BigEndian>(MAGIC_NUMBER_BIG).unwrap();
            self.data.write_u32::<BigEndian>(0).unwrap();
            self.data.write_u64::<BigEndian>(result_length).unwrap();
            self.data.write_u64::<BigEndian>(0).unwrap();
            self.data.write_u64::<BigEndian>(self.size_64).unwrap();
            let mut result = self.data.into_inner();
            result.rotate_right(32);
            result
        }
    }
}

enum IntOrBlock {
    Int(u64),
    Block(u8),
}

struct MarshalingInput {
    data: Cursor<Vec<u8>>,
}

impl MarshalingInput {
    fn new(data: Vec<u8>) -> Self {
        let mut result = Self { data: Cursor::new(data) };
        match result.data.read_u32::<BigEndian>().unwrap() {
            MAGIC_NUMBER_SMALL => result.data.read_exact(&mut [0; 16]).unwrap(),
            MAGIC_NUMBER_BIG => result.data.read_exact(&mut [0; 28]).unwrap(),
            x => panic!("Bad file header: {:#010x}", x),
        };
        result
    }

    fn read_int_or_block(&mut self) -> IntOrBlock {
        match self.data.read_u8().unwrap() {
            x if x >= 0x80 => IntOrBlock::Block((x - 0x80) & 0x0F),
            x if x >= 0x40 => IntOrBlock::Int((x - 0x40) as u64),
            0x00 => IntOrBlock::Int(self.data.read_u8().unwrap() as u64),
            0x01 => IntOrBlock::Int(self.data.read_u16::<BigEndian>().unwrap() as u64),
            0x02 => IntOrBlock::Int(self.data.read_u32::<BigEndian>().unwrap() as u64),
            0x03 => IntOrBlock::Int(self.data.read_u64::<BigEndian>().unwrap()),
            0x08 => IntOrBlock::Block((self.data.read_u32::<BigEndian>().unwrap() & 0xFF) as u8),
            0x13 => IntOrBlock::Block((self.data.read_u64::<BigEndian>().unwrap() & 0xFF) as u8),
            x => panic!("Expected int or block, found byte {:#04x}", x),
        }
    }

    fn read_int(&mut self) -> u64 {
        match self.read_int_or_block() {
            IntOrBlock::Int(int) => int,
            IntOrBlock::Block(_) => panic!("Expected int, found block"),
        }
    }

    fn read_double(&mut self) -> f64 {
        match self.data.read_u8().unwrap() {
            0x0B => self.data.read_f64::<BigEndian>().unwrap(),
            0x0C => self.data.read_f64::<LittleEndian>().unwrap(),
            x => panic!("Expected double, found byte {:#04x}", x),
        }
    }

    fn read_string(&mut self) -> Vec<u8> {
        let len = match self.data.read_u8().unwrap() {
            x if (0x20..0x40).contains(&x) => (x - 0x20) as usize,
            0x09 => self.data.read_u8().unwrap() as usize,
            0x0A => self.data.read_u32::<BigEndian>().unwrap() as usize,
            0x15 => self.data.read_u64::<BigEndian>().unwrap() as usize,
            x => panic!("Expected string, found byte {:#04x}", x),
        };
        let mut result = vec![0; len];
        self.data.read_exact(&mut result).unwrap();
        result
    }
}

fn marshal_output_option<T>(marshaling_output: &mut MarshalingOutput, value: Option<T>, f: fn(&mut MarshalingOutput, T)) {
    match value {
        None => marshaling_output.write_int(0),
        Some(v) => {
            marshaling_output.add_object(1, 1);
            marshaling_output.write_header(1, 0);
            f(marshaling_output, v);
        }
    }
}

fn marshal_input_option<T>(marshaling_input: &mut MarshalingInput, f: fn(&mut MarshalingInput) -> T) -> Option<T> {
    match marshaling_input.read_int_or_block() {
        IntOrBlock::Int(0) => None,
        IntOrBlock::Block(0) => Some(f(marshaling_input)),
        _ => panic!("Unknown tag for list"),
    }
}

fn marshal_output_list<T>(marshaling_output: &mut MarshalingOutput, value: Vec<T>, f: fn(&mut MarshalingOutput, T)) {
    for v in value {
        marshaling_output.add_object(2, 2);
        marshaling_output.write_header(2, 0);
        f(marshaling_output, v);
    }
    marshaling_output.write_int(0);
}

fn marshal_input_list<T>(marshaling_input: &mut MarshalingInput, f: fn(&mut MarshalingInput) -> T) -> Vec<T> {
    let mut result = Vec::new();
    loop {
        match marshaling_input.read_int_or_block() {
            IntOrBlock::Int(0) => break,
            IntOrBlock::Block(0) => result.push(f(marshaling_input)),
            _ => panic!("Unknown tag for option"),
        }
    }
    result
}

fn marshal_output_string(marshaling_output: &mut MarshalingOutput, value: String) {
    marshaling_output.add_object((value.len() as u64 + 4) / 4, (value.len() as u64 + 8) / 8);
    marshaling_output.write_string(value.as_bytes());
}

fn marshal_input_string(marshaling_input: &mut MarshalingInput) -> String {
    String::from_utf8(marshaling_input.read_string()).unwrap()
}

fn marshal_output_bytes(marshaling_output: &mut MarshalingOutput, value: &[u8]) {
    marshaling_output.add_object((value.len() as u64 + 4) / 4, (value.len() as u64 + 8) / 8);
    marshaling_output.write_string(value);
}

fn marshal_input_bytes(marshaling_input: &mut MarshalingInput) -> Vec<u8> {
    marshaling_input.read_string()
}

fn marshal_output_char(marshaling_output: &mut MarshalingOutput, value: char) {
    marshal_output_string(marshaling_output, value.to_string());
}

fn marshal_input_char(marshaling_input: &mut MarshalingInput) -> char {
    let string = marshal_input_string(marshaling_input);
    let mut chars = string.chars();
    let result = chars.next().unwrap();
    assert_eq!(chars.next(), None);
    result
}

fn marshal_output_u32(marshaling_output: &mut MarshalingOutput, value: u32) {
    marshaling_output.write_int(value as u64);
}

fn marshal_input_u32(marshaling_input: &mut MarshalingInput) -> u32 {
    marshaling_input.read_int() as u32
}

fn marshal_output_usize(marshaling_output: &mut MarshalingOutput, value: usize) {
    marshaling_output.write_int(value as u64);
}

fn marshal_input_usize(marshaling_input: &mut MarshalingInput) -> usize {
    marshaling_input.read_int() as usize
}

fn marshal_output_bool(marshaling_output: &mut MarshalingOutput, value: bool) {
    match value {
        false => marshaling_output.write_int(0),
        true => marshaling_output.write_int(1),
    }
}

fn marshal_input_bool(marshaling_input: &mut MarshalingInput) -> bool {
    match marshaling_input.read_int() {
        0 => false,
        1 => true,
        _ => panic!("Unknown tag for bool"),
    }
}

fn marshal_output_token_tree(marshaling_output: &mut MarshalingOutput, value: proc_macro2::TokenTree) {
    match value {
        proc_macro2::TokenTree::Group(group) => {
            marshaling_output.add_object(1, 1);
            marshaling_output.write_header(1, 0);
            marshaling_output.add_object(3, 3);
            marshaling_output.write_header(3, 0);
            match group.delimiter() {
                proc_macro2::Delimiter::Parenthesis => marshaling_output.write_int(0),
                proc_macro2::Delimiter::Brace => marshaling_output.write_int(1),
                proc_macro2::Delimiter::Bracket => marshaling_output.write_int(2),
                proc_macro2::Delimiter::None => marshaling_output.write_int(3),
            }
            marshal_output_token_stream(marshaling_output, group.stream());
            marshal_output_span(marshaling_output, group.span());
        }
        proc_macro2::TokenTree::Ident(ident) => {
            marshaling_output.add_object(1, 1);
            marshaling_output.write_header(1, 1);
            marshal_output_ident(marshaling_output, ident)
        }
        proc_macro2::TokenTree::Punct(punct) => {
            marshaling_output.add_object(1, 1);
            marshaling_output.write_header(1, 2);
            marshaling_output.add_object(3, 3);
            marshaling_output.write_header(3, 0);
            marshal_output_char(marshaling_output, punct.as_char());
            match punct.spacing() {
                proc_macro2::Spacing::Alone => marshaling_output.write_int(0),
                proc_macro2::Spacing::Joint => marshaling_output.write_int(1),
            }
            marshal_output_span(marshaling_output, punct.span());
        }
        proc_macro2::TokenTree::Literal(literal) => {
            marshaling_output.add_object(1, 1);
            marshaling_output.write_header(1, 3);
            marshal_output_literal(marshaling_output, literal)
        }
    }
}

fn marshal_input_token_tree(marshaling_input: &mut MarshalingInput) -> proc_macro2::TokenTree {
    match marshaling_input.read_int_or_block() {
        IntOrBlock::Block(0) => {
            assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
            let delimiter = match marshaling_input.read_int() {
                0 => proc_macro2::Delimiter::Parenthesis,
                1 => proc_macro2::Delimiter::Brace,
                2 => proc_macro2::Delimiter::Bracket,
                3 => proc_macro2::Delimiter::None,
                _ => panic!("Unknown tag for Delimiter"),
            };
            let stream = marshal_input_token_stream(marshaling_input);
            let span = marshal_input_span(marshaling_input);
            let mut group = proc_macro2::Group::new(delimiter, stream);
            group.set_span(span);
            proc_macro2::TokenTree::Group(group)
        }
        IntOrBlock::Block(1) => proc_macro2::TokenTree::Ident(marshal_input_ident(marshaling_input)),
        IntOrBlock::Block(2) => {
            assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
            let char = marshal_input_char(marshaling_input);
            let spacing = match marshaling_input.read_int() {
                0 => proc_macro2::Spacing::Alone,
                1 => proc_macro2::Spacing::Joint,
                _ => panic!("Unknown tag for Spacing"),
            };
            let span = marshal_input_span(marshaling_input);
            let mut punct = proc_macro2::Punct::new(char, spacing);
            punct.set_span(span);
            proc_macro2::TokenTree::Punct(punct)
        }
        IntOrBlock::Block(3) => proc_macro2::TokenTree::Literal(marshal_input_literal(marshaling_input)),
        _ => panic!("Unknown tag for TokenTree"),
    }
}

fn marshal_output_token_stream(marshaling_output: &mut MarshalingOutput, value: proc_macro2::TokenStream) {
    marshal_output_list(marshaling_output, value.into_iter().collect(), marshal_output_token_tree);
}

fn marshal_input_token_stream(marshaling_input: &mut MarshalingInput) -> proc_macro2::TokenStream {
    marshal_input_list(marshaling_input, marshal_input_token_tree).into_iter().collect()
}

fn marshal_output_literal(marshaling_output: &mut MarshalingOutput, value: proc_macro2::Literal) {
    marshaling_output.add_object(2, 2);
    marshaling_output.write_header(2, 0);
    marshal_output_string(marshaling_output, value.to_string());
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_literal(marshaling_input: &mut MarshalingInput) -> proc_macro2::Literal {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let text = marshal_input_string(marshaling_input);
    let span = marshal_input_span(marshaling_input);
    let mut literal = proc_macro2::Literal::from_str(&text).unwrap();
    literal.set_span(span);
    literal
}

fn marshal_output_ident(marshaling_output: &mut MarshalingOutput, value: proc_macro2::Ident) {
    marshaling_output.add_object(2, 2);
    marshaling_output.write_header(2, 0);
    marshal_output_string(marshaling_output, value.to_string());
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_ident(marshaling_input: &mut MarshalingInput) -> proc_macro2::Ident {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let string = marshal_input_string(marshaling_input);
    let span = marshal_input_span(marshaling_input);
    if let Some(strip) = string.strip_prefix("r#") {
        proc_macro2::Ident::new_raw(strip, span)
    } else {
        proc_macro2::Ident::new(&string, span)
    }
}

fn marshal_output_line_column(marshaling_output: &mut MarshalingOutput, value: proc_macro2::LineColumn) {
    marshaling_output.add_object(2, 2);
    marshaling_output.write_header(2, 0);
    marshaling_output.write_int(value.line as u64);
    marshaling_output.write_int(value.column as u64);
}

fn marshal_input_line_column(marshaling_input: &mut MarshalingInput) -> proc_macro2::LineColumn {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let line = marshaling_input.read_int() as usize;
    let column = marshaling_input.read_int() as usize;
    proc_macro2::LineColumn { line, column }
}

fn marshal_output_span(marshaling_output: &mut MarshalingOutput, value: proc_macro2::Span) {
    marshaling_output.add_object(3, 3);
    marshaling_output.write_header(3, 0);
    let source_file = value.source_file().path().to_string_lossy().to_string();
    marshal_output_string(marshaling_output, source_file);
    marshal_output_line_column(marshaling_output, value.start());
    marshal_output_line_column(marshaling_output, value.end());
}

fn marshal_input_span(marshaling_input: &mut MarshalingInput) -> proc_macro2::Span {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let source_file = marshal_input_string(marshaling_input);
    let start = marshal_input_line_column(marshaling_input);
    let end = marshal_input_line_column(marshaling_input);
    proc_macro2::Span::new_custom(&source_file, start, end)
}

fn marshal_output_lit_byte_str(marshaling_output: &mut MarshalingOutput, value: syn::LitByteStr) {
    marshaling_output.add_object(2, 2);
    marshaling_output.write_header(2, 0);
    marshal_output_bytes(marshaling_output, &value.value());
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_lit_byte_str(marshaling_input: &mut MarshalingInput) -> syn::LitByteStr {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let value = marshal_input_bytes(marshaling_input);
    let span = marshal_input_span(marshaling_input);
    syn::LitByteStr::new(&value, span)
}

fn marshal_output_lit_char(marshaling_output: &mut MarshalingOutput, value: syn::LitChar) {
    marshaling_output.add_object(2, 2);
    marshaling_output.write_header(2, 0);
    marshal_output_char(marshaling_output, value.value());
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_lit_char(marshaling_input: &mut MarshalingInput) -> syn::LitChar {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let value = marshal_input_char(marshaling_input);
    let span = marshal_input_span(marshaling_input);
    syn::LitChar::new(value, span)
}

fn marshal_output_lit_byte(marshaling_output: &mut MarshalingOutput, value: syn::LitByte) {
    marshaling_output.add_object(2, 2);
    marshaling_output.write_header(2, 0);
    marshaling_output.write_int(value.value() as u64);
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_lit_byte(marshaling_input: &mut MarshalingInput) -> syn::LitByte {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let value = marshaling_input.read_int() as u8;
    let span = marshal_input_span(marshaling_input);
    syn::LitByte::new(value, span)
}

fn marshal_output_lit_float(marshaling_output: &mut MarshalingOutput, value: syn::LitFloat) {
    marshaling_output.add_object(3, 3);
    marshaling_output.write_header(3, 0);
    marshal_output_string(marshaling_output, value.base10_digits().to_string());
    marshal_output_string(marshaling_output, value.suffix().to_string());
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_lit_float(marshaling_input: &mut MarshalingInput) -> syn::LitFloat {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let digits = marshal_input_string(marshaling_input);
    let suffix = marshal_input_string(marshaling_input);
    let span = marshal_input_span(marshaling_input);
    syn::LitFloat::new(&format!("{}{}", digits, suffix), span)
}

fn marshal_output_lit_int(marshaling_output: &mut MarshalingOutput, value: syn::LitInt) {
    marshaling_output.add_object(3, 3);
    marshaling_output.write_header(3, 0);
    marshal_output_string(marshaling_output, value.base10_digits().to_string());
    marshal_output_string(marshaling_output, value.suffix().to_string());
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_lit_int(marshaling_input: &mut MarshalingInput) -> syn::LitInt {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let digits = marshal_input_string(marshaling_input);
    let suffix = marshal_input_string(marshaling_input);
    let span = marshal_input_span(marshaling_input);
    syn::LitInt::new(&format!("{}{}", digits, suffix), span)
}

fn marshal_output_lit_str(marshaling_output: &mut MarshalingOutput, value: syn::LitStr) {
    marshaling_output.add_object(2, 2);
    marshaling_output.write_header(2, 0);
    marshal_output_string(marshaling_output, value.value());
    marshal_output_span(marshaling_output, value.span());
}

fn marshal_input_lit_str(marshaling_input: &mut MarshalingInput) -> syn::LitStr {
    assert!(matches!(marshaling_input.read_int_or_block(), IntOrBlock::Block(0)));
    let value = marshal_input_string(marshaling_input);
    let span = marshal_input_span(marshaling_input);
    syn::LitStr::new(&value, span)
}

#[ocaml::func]
pub fn native_token_stream_of_string(x: &str) -> ocaml::Value {
    let mut marshaling_output = MarshalingOutput::new();
    marshal_output_token_stream(&mut marshaling_output, proc_macro2::TokenStream::from_str(x).unwrap());
    unsafe { ocaml::Value::bytes(&marshaling_output.finish()) }
}

#[ocaml::func]
pub fn native_string_of_token_stream(x: &[u8]) -> String {
    let mut marshaling_input = MarshalingInput::new(x.into());
    marshal_input_token_stream(&mut marshaling_input).to_string()
}

#[ocaml::func]
pub fn native_span_of_token_stream(x: &[u8]) -> ocaml::Value {
    let mut marshaling_input = MarshalingInput::new(x.into());
    let mut marshaling_output = MarshalingOutput::new();
    marshal_output_span(&mut marshaling_output, marshal_input_token_stream(&mut marshaling_input).span());
    unsafe { ocaml::Value::bytes(&marshaling_output.finish()) }
}

include!("generated.rs");

// /// A function that is necessary to get `dune utop` to work for some reason.
// #[ocaml::func]
// pub fn caml_startup() {}
