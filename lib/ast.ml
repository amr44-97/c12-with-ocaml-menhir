(* No .mli needed for AST usually, as it's just types *)
(* 1. Binary Operators *)

exception Error_Message of string * Lexing.position * Lexing.position

type binop = 
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Leq | Geq
  | And | Or | Bit_And | Bit_Or | Bit_Xor | Shl | Shr
[@@deriving show]

(* 2. Unary Operators *)
type unaryop = 
  | Negation       (* -x *)
  | Bool_Not       (* !x *)
  | Bit_Not    (* ~x *)
  | Deref     (* *x *)
  | Address_Of    (* &x *)
[@@deriving show]

(* 3. Types *)
type typ =
  | TInt                    (* int *)
  | TShort                  (* short *)
  | TLong                   (* long *)
  | TFloat                  (* float *)
  | TDouble                 (* float *)
  | TChar                   (* char *)
  | TBool                   (* bool *)
  | TVoid                   (* void *)
  | TType                   (* type (for generics) *)
 (*  | TNamed of string *)        (* struct/enum names or generic T *)
  | TNamed of string list       (* struct/enum names or generic T *)
  | TPtr of typ             (* T* *)
  | TOptional of typ        (* T? *)
  | TArray of typ * int option (* T[] or T[10] *)
  | TSlice of typ * int option (* T[] or T[10] *)
[@@deriving show]


(* 4. Expressions *)

type expr =
  | Variable of string
  | Identifier of string
  | NumberLiteral of int
  (* | FloatLit of float *)
  | StringLiteral of string
  | CharLiteral of string
  | BoolLiteral of bool
  | Null
  | Id of string
  | BinOp of expr * binop * expr
  | UnaryOp of unaryop * expr
  | Call of  expr * expr list        (* func(a, b) *)
  | Member of expr * string           (* obj.field *)
  | Cast of typ * expr                (* (int) x *)
  | SizeOf of typ                     (* sizeof(int) *)
  | ArrayAccess of expr * expr        (* arr[i] *)
  | StructInit of string * (string * expr) list (* Point { .x = 1, .y = 2 } *)
  | Switch of expr * (expr list * stmt list) list * stmt list option 
    (* switch(x) { 1,2 => { ... }, else => { ... } } *)
[@@deriving show]

and id_expr = 
  | Identifier of string
  | Member of expr * string           (* obj.field *)
[@@deriving show]

(* 5. Statements *)
and stmt =
  | Decl of typ * string * expr option  (* int x = 5; *)
  | Let of string * expr
  | Const of string * expr
  | Auto of string * expr
  | Assign of expr * expr               (* x = 10; *)
  | Assign_Add of expr * expr
  | Assign_Sub of expr * expr
  | Assign_Mul of expr * expr
  | Assign_Div of expr * expr
  | Assign_Mod of expr * expr
  | Assign_Or of expr * expr
  | Assign_Xor of expr * expr
  | Assign_Shl of expr * expr
  | Assign_Shr of expr * expr
  | Expr of expr                        (* func(); *)
  | Block of stmt list                  (* { ... } *)
  | If of expr * stmt * stmt option     (* if (x) ... else ... *)
  | While of expr * stmt                (* while (x) ... *)
  | For of stmt option * expr option * stmt option * stmt (* for(;;) *)
  | Return of expr option               (* return x; *)
  | Break
  | Continue
  | Defer of stmt                       (* defer free(x); *)
  | Call of string * expr list
[@@deriving show]

(* 6. Top Level Declarations *)
type func_param = { p_type: typ; p_name: string }
[@@deriving show]

type top_level =
  | FuncDef of typ * string * func_param list  * stmt (* int add(...) { } *)
  | FuncProto of typ * string * func_param list      (*int add();*)
  | StructDef of string * (string * typ * expr option) list        (* struct P { int x; }; *)
  | StructDecl of string                             (* struct P ; *)
  | EnumDef of string * string list                  (* enum Color { Red }; *)
  | UnionDef of string * string option * string list                  (* enum Color { Red }; *)
  | DistinctDef of string * typ                      (* distinct Id = int; *)
  | GlobalDecl of typ * string * expr option         (* int global = 10; *)
  | Include of string list * string option * bool           (* include "io" as io; *)
  | MacroDef of string * string list * stmt          (* macro m(args) { ... } *)
[@@deriving show]

type program = top_level list
[@@deriving show]
