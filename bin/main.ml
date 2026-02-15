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
    (* Set the filename in lexbuf so positions are correct 
      lexbuf is mutable of type position *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    (* We pass 'lexbuf' only for error position reporting *)
    let ast = Parser.program (* token_provider *) Lexer.read lexbuf in
    (* E. Print AST (using ppx_deriving.show) *)
    print_endline "--- Abstract Syntax Tree ---";
    print_endline (Ast.show_program ast) 
  
with
  | Sys_error msg -> 
      eprintf "Error opening file: %s\n" msg
  | Lexer.Error msg ->
    let source_code = read_file filename in
    let lexbuf = Lexing.from_string source_code in
      (* eprintf "Lexer Error: %s\n" msg *)
    Diagnostic.print_error filename source_code lexbuf ("Lexer Error: " ^ msg)
  (* | Parser.Error -> *)
  (*   let source_code = read_file filename in *)
  (*   let lexbuf = Lexing.from_string source_code in *)
  (*   Diagnostic.print_error filename source_code lexbuf "Syntax Error" *)
  | Failure msg ->
      eprintf "Error: %s\n" msg

  (* Catch your CUSTOM error *)
  | Ast.Error_Message (msg, start_pos, end_pos) ->
    let source = read_file filename in
    let lexbuf = Lexing.from_string source in
       (* We construct a temporary lexbuf at the right pos to trick the printer, 
          or update your print_diagnostic to take positions directly.
          For simplicity, let's just use the start_pos. *)
       lexbuf.lex_curr_p <- start_pos;
       Diagnostic.print_diagnostic
         Diagnostic.Syntax_Error 
         Diagnostic.Error 
         msg 
         lexbuf 
         source
  (* Catch the GENERIC error (fallback) *)
  | Parser.Error ->
    let source = read_file filename in
    let lexbuf = Lexing.from_string source in
       Diagnostic.print_diagnostic 
         Diagnostic.Syntax_Error 
         Diagnostic.Error 
         "Syntax Error (Expression expected or illegal character)" 
         lexbuf 
         source





