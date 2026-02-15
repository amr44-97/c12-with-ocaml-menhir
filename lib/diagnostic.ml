(* lib/diagnostic.ml *)
open Printf
open Lexing

(* OCaml Concept: 'option'
   This is like Rust's Option<T>. It can be 'Some value' or 'None'.
   We use it here because we might not always have a specific range to point to.
*)

(* Helper function: get_line_content
   Takes the full source code and a line number, and extracts just that line.
*)
let get_line_content source line_num =
  let lines = String.split_on_char '\n' source in
  (* List.nth returns the Nth element. OCaml lists are 0-indexed, so we sub 1 *)
  if line_num <= List.length lines && line_num > 0 then
    Some (List.nth lines (line_num - 1))
  else
    None

(* Main function: print_error
   Arguments:
   - filename: string
   - source: string (the whole file content)
   - lexbuf: the lexer buffer (contains the current position)
   - msg: the error message string
*)
let print_error filename source lexbuf msg =
  (* 1. Get positions from the lexbuf *)
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in

  (* 2. Print the Header (Rust style) *)
  (* \027[1;31m starts Red Bold text. \027[0m resets color. *)
  eprintf "\027[1;31mError:\027[0m %s\n" msg;
  eprintf "   \027[1;34m-->\027[0m %s:%d:%d\n" filename line col;
  
  (* 3. Print the Snippet *)
  match get_line_content source line with
  | Some line_text ->
      eprintf "    \027[1;34m|\027[0m\n";
      (* Print the line number and code *)
      eprintf "\027[1;34m%3d |\027[0m %s\n" line line_text;
      
      (* Print the pointer (^) *)
      (* String.make N ' ' creates a string of N spaces *)
      let spaces = String.make col ' ' in
      eprintf "    \027[1;34m|\027[0m %s\027[1;31m^\027[0m\n\n" spaces
  | None ->
      eprintf "    (Unable to retrieve source line)\n\n"


(* 1. Define Severity Levels *)
(* OCaml Concept: Variant Type
   This is exactly like Rust's enum. 
   It can be one of these three tags. *)
type severity = 
  | Error 
  | Warning 
  | Note

(* 2. Define Error Sources *)
type error_kind = 
  | Lexical_Error
  | Syntax_Error
  | Semantic_Error
  | Internal_Error

(* 3. Helper to get color and string prefix *)
(* OCaml Concept: Pattern Matching
   'function' is a shortcut for 'match argument with'.
   We return a tuple: (color_code, text_prefix) *)
let header_of_severity = function
  | Error   -> ("\027[1;31m", "[Error]:")   (* Red *)
  | Warning -> ("\027[1;33m", "[Warning]:") (* Yellow *)
  | Note    -> ("\027[1;36m", "[Note]:")    (* Cyan *)

let string_of_kind = function
  | Lexical_Error  -> "Lexical Error"
  | Syntax_Error   -> "Syntax Error"
  | Semantic_Error -> "Type Error"
  | Internal_Error -> "Internal Compiler Error"

(* 4. The Diagnostic Printer *)
let print_diagnostic (kind : error_kind) (sev : severity) (msg : string) 
                     (lexbuf : Lexing.lexbuf) (source : string) =
  
  (* A. Extract position info *)
  let pos = lexbuf.lex_curr_p in
  let filename = pos.pos_fname in
  let line = pos.pos_lnum in
  (* Column calculation: Current absolute offset - Absolute offset of line start *)
  let col = pos.pos_cnum - pos.pos_bol in

  (* B. Get formatting info *)
  let (color, level_str) = header_of_severity sev in
  let kind_str = string_of_kind kind in

  (* C. Print Header *)
  (* e.g. "Error[Syntax Error]: Missing semicolon" *)
  eprintf "%s%s\027[0m[%s]: %s\n" color level_str kind_str msg;
  
  (* D. Print Location Link *)
  (* e.g. "   --> test.c12:10:5" *)
  eprintf "   \027[1;34m-->\027[0m %s:%d:%d\n" filename line col;

  (* E. Print Code Snippet *)
  let lines = String.split_on_char '\n' source in
  let line_idx = line - 1 in
  
  if line_idx >= 0 && line_idx < List.length lines then begin
    let line_content = List.nth lines line_idx in
    
    eprintf "    \027[1;34m|\027[0m\n";
    eprintf "\027[1;34m%3d |\027[0m %s\n" line line_content;
    
    (* Pointer Logic: make string of spaces then put ^ *)
    let pointer = String.make col ' ' ^ "\027[1;31m^\027[0m" in
    eprintf "    \027[1;34m|\027[0m %s\n\n" pointer
  end else begin
    eprintf "    (Context unavailable)\n\n"
  end


