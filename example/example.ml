let read_to_string (path : string) : string =
  let channel = open_in path in
  let result = really_input_string channel (in_channel_length channel) in
  close_in channel;
  result

let rustfmt (code : string) : string = Shexp_process.eval
    (Shexp_process.pipe
       (Shexp_process.pipe
          (Shexp_process.print code)
          (Shexp_process.run "rustfmt" []))
       Shexp_process.read_all)

let string_of_span (span : Ocaml_rust_parser_generator.rs_span) : string =
  Printf.sprintf "Span(\"%s\", %d:%d..%d:%d)"
    span.rs_source_file
    span.rs_start.rs_line span.rs_start.rs_column
    span.rs_end.rs_line span.rs_end.rs_column

let _ =
  let code = read_to_string "example/code.rs" in
  print_endline "Original code:";
  print_string code;
  print_newline ();
  print_endline "After turning into a rs_token_stream and back to string:";
  print_string (rustfmt (Ocaml_rust_parser_generator.string_of_token_stream ((Ocaml_rust_parser_generator.token_stream_of_string code))));
  print_newline ();
  print_endline "After turning into a rs_file and back to string:";
  print_string (rustfmt (Ocaml_rust_parser_generator.generate_file_to_string ((Ocaml_rust_parser_generator.parse_file_from_string code))));
  print_newline ();
  print_endline "The first attribute in the file:";
  print_string (rustfmt (Ocaml_rust_parser_generator.generate_attribute_to_string (List.hd ((Ocaml_rust_parser_generator.parse_file_from_string code).rs_attrs))));
  print_newline ();
  print_string "Count of the items in the file: ";
  print_int (List.length ((Ocaml_rust_parser_generator.parse_file_from_string code).rs_items));
  print_newline ();
  print_string "Spans of the items in the file: ";
  print_string (String.concat ", " (List.map string_of_span (List.map Ocaml_rust_parser_generator.span_of_item ((Ocaml_rust_parser_generator.parse_file_from_string code).rs_items))));
  print_newline ();
  print_string "Everything is preserved when tuning rs_file into a rs_token_stream and back: ";
  print_string (if (Ocaml_rust_parser_generator.parse_file_from_token_stream (Ocaml_rust_parser_generator.generate_file_to_token_stream (Ocaml_rust_parser_generator.parse_file_from_string code)) = Ocaml_rust_parser_generator.parse_file_from_string code) then "true" else "false");
  print_newline ();
