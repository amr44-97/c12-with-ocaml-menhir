open Printf
open Cortex
open Lexer

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let rec lex_all_tokens lexbuf =
  let token = Lexer.read lexbuf in
  if token = Parser.EOF then
    [Parser.EOF]
  else
    token :: lex_all_tokens lexbuf

let () =
    let filename = Sys.argv.(1) in
    if Array.length Sys.argv < 2 then begin
        printf "Usage: %s <filename.c12>\n" Sys.argv.(0);
        exit 1
      end;
try
    (* A. Read File *)
    let source_code = read_file filename in
    let lexbuf = Lexing.from_string source_code in
    Diagnostic.current_source := source_code;
    (* Set the filename in lexbuf so positions are correct 
      lexbuf is mutable of type position *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    (* We pass 'lexbuf' only for error position reporting *)
    let ast = Parser.program (* token_provider *) Lexer.read lexbuf in
    Diagnostic.report_all_warnings source_code;

    (* E. Print AST (using ppx_deriving.show) *)
    
    (* print_endline "--- Abstract Syntax Tree ---"; *)
    (* print_endline (Ast.show_program ast);  *)
 
    let c_code = Codegen.generate ast in
    (* 2. Print or Save It *)
    print_endline "--- Generated C Code ---";
    (* print_endline c_code; *)

    (* Optional: Save to file *)
    let out_chan = open_out "output.c" in
    output_string out_chan c_code;
    close_out out_chan;
    printf "\n[Success] Wrote output to output.c\n" 

with
    | Sys_error msg -> 
      eprintf "Error opening file: %s\n" msg
    | Failure msg ->
      eprintf "Error: %s\n" msg
    | Lexer.Error msg ->
    let source = read_file filename in
    let lexbuf = Lexing.from_string source in
      Diagnostic.print_diagnostic
        Diagnostic.Lexical_Error
        Diagnostic.Error
        msg
        lexbuf.lex_curr_p (* Pass position directly *)
        source

    | Ast.Error_Message (msg, start_pos, _) ->
        let source = read_file filename in
      Diagnostic.print_diagnostic 
        Diagnostic.Syntax_Error 
        Diagnostic.Error 
        msg 
        start_pos (* Pass the saved position *)
        source

    | Parser.Error ->
    let source = read_file filename in
    let lexbuf = Lexing.from_string source in
      Diagnostic.print_diagnostic
        Diagnostic.Syntax_Error
        Diagnostic.Error
        "Unexpected token"
        lexbuf.lex_curr_p
        source








