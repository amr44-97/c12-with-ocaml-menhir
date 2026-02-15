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
    
    (* B. Lex Everything (Buffered Strategy) *)
    (* This runs your Lexer logic (flags/tables) completely first *)
    (* let all_tokens = lex_all_tokens lexbuf in *)
    
    (* Optional: Debug Print Tokens *)
    (* List.iter (fun t -> printf "%s " (Parser.token_to_string t)) all_tokens; 
    printf "\n\n"; 
    *)

    (* C. Create a Token Provider for Menhir *)
    (* Menhir needs a function () -> token, so we make one that pops from our list *)
    (* let token_list_ref = ref all_tokens in *)
    (* let token_provider _lexbuf = *)
    (*   match !token_list_ref with *)
    (*   | [] -> Parser.EOF *)
    (*   | t :: rest ->  *)
    (*       token_list_ref := rest;  *)
    (*       t *)
    (* in *)

    (* D. Parse *)
    (* We pass 'lexbuf' only for error position reporting *)
    let ast = Parser.program (* token_provider *) Lexer.read lexbuf in

    (* E. Print AST (using ppx_deriving.show) *)
    print_endline "--- Abstract Syntax Tree ---";
    print_endline (Ast.show_program ast)

  with
  | Sys_error msg -> 
      eprintf "Error opening file: %s\n" msg
  | Lexer.Error msg ->
      eprintf "Lexer Error: %s\n" msg
  | Parser.Error ->
      eprintf "Parser Error: Syntax error. \n" 
  | Failure msg ->
      eprintf "Error: %s\n" msg

