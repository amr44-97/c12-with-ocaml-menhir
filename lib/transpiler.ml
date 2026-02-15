open Ast

(* let string_of_op = function *)
(*   | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%" *)

(* let rec transpile_expr = function *)
(*   | NumberLiteral i -> string_of_int i *)
(*   | Variable x -> x *)
(*   | BinOp (e1, op, e2) ->  *)
(*       Printf.sprintf "(%s %s %s)" (transpile_expr e1) (string_of_op op) (transpile_expr e2) *)
(*   | _ -> Printf.eprintf "failed"; *)


let string_of_program stmts =
  (* let body = String.concat "\n  " (List.map transpile_stmt stmts) in *)
  Printf.sprintf 
"include \"stdio.h\"
int main() {
  Hello World
  return 0;
}" 
