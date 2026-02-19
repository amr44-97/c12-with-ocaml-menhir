open Ast 
open Printf

let buffer = Buffer.create (1024 * 64)


(* appends string `str` to buffer *)
let emit str = Buffer.add_string buffer str
let emit_f fmt = ksprintf emit fmt


let rec gen_type = function 
  | TInt -> "int"                    (* int *)
  | TShort -> "short"                 (* short *)
  | TLong  -> "long"                 (* long *)
  | TFloat -> "float"                 (* float *)
  | TDouble  -> "double"               (* float *)
  | TChar   -> "char"                (* char *)
  | TBool   -> "bool"                (* bool *)
  | TVoid   -> "void"                (* void *)
  (* | TType                   (* type (for generics) *) *)
 (*  | TNamed of string *)        (* struct/enum names or generic T *)
  | TNamed path -> String.concat "." path  (* struct/enum names or generic T *)
  | TPtr t -> sprintf "%s*" (gen_type t)              (* T* *)
  (* | TOptional of typ        (* T? *) *)
  (* | TArray of typ * int option (* T[] or T[10] *) *)
  | TArray(t, arr_len) -> 
          begin match arr_len with 
            | None -> sprintf "%s[]" (gen_type t)
            | Some l -> sprintf "%s[%d]" (gen_type t) l 
          end 
  (* | TSlice of typ * int option (* T[] or T[10] *) *)
  | _ -> ""

let gen_binop = function 
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Eq -> "="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | And -> "&&"
    | Or -> "||"
    | Bit_And -> "&"
    | Bit_Or -> "|" 
    | Bit_Xor -> "^"
    | Shl -> "<<"
    | Shr -> ">>"


let gen_unop = function 
      | Negation  -> "-"
      | Bool_Not  -> "!"
      | Bit_Not   -> "~" 
      | Deref     -> "*"
      | Address_Of -> "&" 

let rec gen_expr = function 
    | Variable _var -> _var 
    | Identifier id -> id
    | NumberLiteral num -> string_of_int num
    | StringLiteral str -> sprintf "\"%s\"" str 
    (* NOTE: char literal is a string*)
    | CharLiteral ch -> sprintf "'%s'" ch
    | BoolLiteral bl -> string_of_bool bl 
    | BinOp (lhs, op, rhs) -> 
            sprintf "(%s %s %s)" (gen_expr lhs) (gen_binop op) (gen_expr rhs)
    | Call(e,args) -> 
            let args_str = List.map gen_expr args |> String.concat ", " in
            sprintf "%s(%s)" (gen_expr e) args_str
    | Member(parent, field) -> 
            sprintf "%s.%s" (gen_expr parent) field
    | Cast(ty,e) -> 
            sprintf "(%s)%s" (gen_type ty) (gen_expr e)
    | SizeOf(ty) -> 
            sprintf "sizeof(%s)" (gen_type ty)
    | ArrayAccess(array_name,index) -> 
            sprintf "%s[%s]" (gen_expr array_name) (gen_expr index)
    (* | StructInit (t, init_list) ->  *)
    (*         let init_list_str =  *)
    (*             List.map (fun elem -> sprintf ".%s = %s" (init_list.p_name) ) init_list *)
    | _ -> ""
let rec gen_stmt = function 
    | Decl(ty,name, init_opt) -> 
            let d_type = gen_type ty in 
            begin match init_opt with
            | None -> emit_f "    %s %s;\n" d_type name 
            | Some expr -> emit_f "    %s %s = %s;\n" d_type name (gen_expr expr)
            end

    | Return e ->
        begin match e with 
            | None ->  emit_f   "    return;\n"
            | Some e ->  emit_f "    return %s;\n" (gen_expr e)
            end
    | Expr e -> 
      emit_f "    %s;\n" (gen_expr e)

    |   Block stmts ->
      emit "{\n";
      List.iter gen_stmt stmts;
      emit "}\n"

    | Defer _ -> 
      emit "    /* TODO: Implement defer mechanism */\n"
  
    | If (cond, then_branch, else_branch) ->
      emit_f "    if (%s) " (gen_expr cond);
      gen_stmt then_branch;
      begin match else_branch with
      | Some stmt -> 
          emit " else ";
          gen_stmt stmt
      | None -> emit "\n"
      end

    | _ -> emit_f ""

(* 6. Top Level Generator *)
let gen_top_level = function
  | FuncDef (ret_type, name, params, body) ->
      (* Convert params [(Int, "x")] -> "int x" *)
      let param_str = 
        List.map (fun p -> sprintf "%s %s" (gen_type p.p_type) p.p_name) params 
        |> String.concat ", " 
      in
      emit_f "\n%s %s(%s) " (gen_type ret_type) name param_str;
      gen_stmt body; (* Body is usually a Block *)
      emit "\n"

  | StructDef (name, fields) ->
      emit_f "struct %s {\n" name;
      List.iter (fun (n, t, _) -> 
        (* Ignoring default values for C structs for now *)
        emit_f "    %s %s;\n" (gen_type t) n
      ) fields;
      emit "};\n"

  | Include  (module_name, _) -> 
          emit_f "#include %s\n" ("\"" ^ module_name ^ "\"" )

  | _ -> emit "/* Unknown Top Level */\n"

(* 7. Main Entry Point *)
let generate (prog : program) =
  Buffer.clear buffer;
  
  (* Add standard C headers *)

  List.iter gen_top_level prog;
  Buffer.contents buffer










