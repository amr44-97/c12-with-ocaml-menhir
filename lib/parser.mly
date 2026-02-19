%{
  open Ast
  open Printf
  open Lexing

    (* Helper to raise it using the current location *)
    let fail_syntax msg start_pos end_pos =
        raise (Error_Message (msg, start_pos ,end_pos ))
%}

/* Tokens (The vocabulary) */
%token <int> NUMBER_LITERAL
%token <string> IDENTIFIER
%token <string> CHAR_LITERAL 
%token <string> STRING_LITERAL 
%token TRUE FALSE NULL
%token PLUS MINUS MUL DIV MOD SHL SHR EQUAL
%token ADD_ASSIGN
%token SUB_ASSIGN
%token MUL_ASSIGN
%token DIV_ASSIGN
%token MOD_ASSIGN
%token OR_ASSIGN 
%token AND_ASSIGN
%token XOR_ASSIGN
%token SHL_ASSIGN
%token SHR_ASSIGN
%token EQUAL_EQUAL NOT_EQUAL 
%token LESS_THAN LESS_OR_EQUAL 
%token GREATER_THAN GREATER_OR_EQUAL 
%token INCREMENT DECREMENT 
%token BANG 
%token ARROW
%token AMPERSAND AMPERSAND2 
%token PIPE PIPE_PIPE 
%token CARET TILDE COLON QUESTION_MARK COMMA SEMICOLON 
%token DOT ELLIPSIS2 ELLIPSIS3 
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE 
%token LET IMPL CONST AUTO BREAK WHILE CONTINUE DO ELSE FOR GOTO IF EXTERN
%token INLINE RETURN SIZEOF STATIC STRUCT ENUM UNION SWITCH TYPEDEF
%token VOID SIGNED UNSIGNED INT CHAR DOUBLE FLOAT SHORT LONG BOOL
%token AS MACRO DEFER INCLUDE
%token EOF

(*  --- Precedence (Lowest to Highest) ---  *)
%right ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN OR_ASSIGN AND_ASSIGN XOR_ASSIGN SHL_ASSIGN SHR_ASSIGN
%left PIPE_PIPE
%left AMPERSAND2
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left EQUAL NOT_EQUAL
%left LESS_THAN GREATER_THAN LESS_OR_EQUAL GREATER_OR_EQUAL
%left SHL SHR
%left PLUS MINUS
%left MUL DIV MOD
%right NOT BIT_NOT TYPE_CAST /* Unary */
%left DOT  /* Access */


(* /* Entry Point */ *)
%start <Ast.program> program

%%

(* /* Grammar Rules */ *)
program:
    | decls = list(top_level); EOF { decls }



id_name:
    | id = IDENTIFIER { id }
    | AS  { "as" }

(*  --- Top Level Declarations ---  *)
(*  Function: int add(int x) { ... }  *)
(*  Function prototype */ *)
(*  Struct: struct Point { int x; int y; };  *)
(*  Enum: enum Color { Red, Blue };  *)
(*  Import: include "math" as m; *)
top_level:
  | ret = typ; name = IDENTIFIER; LPAREN; params = separated_list(COMMA, param) ; RPAREN; body = block_stmt
    { FuncDef(ret, name, params, body) }
  
  | ret = typ; name = IDENTIFIER; LPAREN; params = separated_list(COMMA, param); RPAREN; SEMICOLON
    { FuncProto(ret, name, params) }
  
  | STRUCT; name = IDENTIFIER; SEMICOLON  { StructDecl(name) }
  
  (* | STRUCT; name = IDENTIFIER; LBRACE; fields = separated_list(COMMA,struct_field); RBRACE  *)
  (*   { StructDef(name, fields) } *)

  | STRUCT; name = IDENTIFIER; LBRACE; fields = list(struct_field); RBRACE; 
      { StructDef(name, fields) }
  (* | STRUCT; name = IDENTIFIER; LBRACE; fields = separated_list(COMMA,struct_field); option(COMMA); RBRACE  *)
  (*   { StructDef(name, fields) } *)
  (*  *)
  (* | STRUCT; name = IDENTIFIER; LBRACE; fields = list(struct_field); COMMA; RBRACE  *)
  (*   { StructDef(name, fields) } *)

  | ENUM; name = IDENTIFIER; LBRACE; variants = separated_list(COMMA, IDENTIFIER); RBRACE 
    { EnumDef(name, variants) }

  | UNION; name = IDENTIFIER; enum_tag = option(IDENTIFIER) ;LBRACE; variants = separated_list(COMMA, IDENTIFIER); RBRACE 
    { UnionDef(name, enum_tag, variants) }

 | inc = c_include { inc }

normal_include:
  | INCLUDE; include_data = import_path ; alias = option(preceded(AS, IDENTIFIER)); SEMICOLON
    {   
        (* let (path, is_wildcard) = include_data in *)
        (* Include(path, alias, is_wildcard) *)
    }

c_include:
    | INCLUDE; path = STRING_LITERAL ; alias = option(preceded(AS, IDENTIFIER)); SEMICOLON
    {   
        Include(path, alias)
    }


include_stmt:
  | path =  separated_nonempty_list(DOT, IDENTIFIER) { (path,false) }
  | path =  separated_nonempty_list(DOT, IDENTIFIER); DOT; MUL { (path,true) }

import_path:
  (* /* Case 1: End of path "io" */ *)
  | id = IDENTIFIER 
    { ([id], false) }

  (* /* Case 2: Wildcard "io.*" */ *)
  | id = IDENTIFIER; DOT; MUL 
    { ([id], true) }

  | id = IDENTIFIER; DOT 
    { 
        let msg = sprintf "expected Identifier or wildcard after module name `%s`" id in 
        fail_syntax msg $startpos $endpos
    }
  (* /* Case 3: Recursion "std. ..." */ *)
  | id = IDENTIFIER; DOT; rest = import_path 
    { 
      let (tail, is_wildcard) = rest in
      (id :: tail, is_wildcard) 
    }



(* /* --- Function Parameters --- */ *)
param:
  | t = typ; name = IDENTIFIER; default_v = option(preceded(EQUAL,expr)) { { p_type = t; p_name = name; default_value = default_v } }

(* /* --- Struct Fields --- */ *)
struct_field:
  | t = typ; name = IDENTIFIER; expr = option(preceded(EQUAL,expr)); SEMICOLON { (name, t, expr) }

(* /* A list of fields that allows an optional trailing comma */ *)
struct_fields:
  |  { [] }
  | f = struct_field 
      { [f] }
  | f = struct_field; COMMA; tail = struct_fields 
      { f :: tail }

(*  Helper: "x" or "x = 5"  *)
struct_decl_item:
  | name = IDENTIFIER; init = option(preceded(EQUAL, expr)) 
    { (name, init) }

(*  Helper: "float x, y = 5"  *)
struct_decl_line:
  | t = typ; items = separated_nonempty_list(COMMA, struct_decl_item)
    { List.map (fun (n, i) -> (n, t, i)) items }

(* Helper: The whole body "float x, y, int z," *)
struct_body:
  |  { [] }
  | line = struct_decl_line 
      { line }
  | line = struct_decl_line; COMMA; tail = struct_body 
      { line @ tail }



(*  --- Types ---  *)
  (* | TYPE    { TType } /* Generic 'type T' */ *)
  (* /* Pointers: int* */ *)
  (* /* Optionals: int? */ *)
  (* /* Arrays: int[] or int[10..n] */ *)
  (* /* Slice: int[0..] or int[0..n] */ *)
typ:
  | INT    { TInt }
  | FLOAT  { TFloat }
  | DOUBLE { TDouble }
  | SHORT  { TShort }
  | LONG   { TLong }
  | CHAR   { TChar }
  | BOOL   { TBool }
  | VOID   { TVoid }
  
  (* | name = IDENTIFIER  *)
  (*   { TNamed name } *)
  
  | path = separated_nonempty_list(DOT, IDENTIFIER)  
    { TNamed(path) }
  
  | t = typ; MUL 
    { TPtr t }
  
  | t = typ; QUESTION_MARK 
    { TOptional t }
  
  | t = typ; LBRACKET; size = option(NUMBER_LITERAL); RBRACKET 
    { TArray(t, size) }
  
  | t = typ; LBRACKET; size = option(NUMBER_LITERAL); option(ELLIPSIS2); option(NUMBER_LITERAL) ; RBRACKET 
    { TSlice(t, size) }


(*  --- Statements --- */ *)
  (*  Assignment: x = 10;  *)
  (*  Expression Statement: func();  *)
  (*  Block: { ... }  *)
  (*  If: if (x) ... else ...  *)
  (*  While: while (x) ...  *)
  (*  Return: return x;  *)
  (*  Defer: defer free(x);  *)
  (*  Switch (Zig Style): switch (x) { 1 => ..., 2, 3 => ... }  *)
stmt:
  (*  Variable Declaration: int x = 5;  *)
  | ty = typ; name = IDENTIFIER; init = option(preceded(EQUAL, expr)); SEMICOLON
    { 
        if ty = TVoid then 
            let msg = sprintf "illegal void type usage" in 
            fail_syntax msg $startpos $endpos
        else 
            Decl(ty, name, init) 
    }

  | LET; name = IDENTIFIER; init = preceded(EQUAL, expr); SEMICOLON
    { Let(name, init) }
  
  | CONST; name = IDENTIFIER; init = preceded(EQUAL, expr); SEMICOLON
    { Const(name, init) }
  
  | AUTO; name = IDENTIFIER; init = preceded(EQUAL, expr); SEMICOLON
    { Auto(name, init) }

  | e1 = expr; EQUAL; e2 = expr; SEMICOLON { Assign(e1,Eq, e2) }
  | e1 = expr; PLUS; EQUAL; e2 = expr { Assign(e1,Add,e2)}
  | e1 = expr; MINUS; EQUAL; e2 = expr { Assign(e1,Sub,e2)}
  | e1 = expr; MUL; EQUAL; e2 = expr { Assign(e1,Mul,e2)}
  | e1 = expr; DIV; EQUAL; e2 = expr { Assign(e1,Div,e2)}
  | e1 = expr; MOD; EQUAL; e2 = expr { Assign(e1,Mod,e2)}
  | e1 = expr; PIPE;  EQUAL; e2 = expr { Assign(e1,Bit_Or,e2)}
  | e1 = expr; AMPERSAND;  EQUAL; e2 = expr { Assign(e1,Bit_And,e2)}
  | e1 = expr; CARET; EQUAL; e2 = expr { Assign(e1,Bit_Xor,e2)}
  | e1 = expr; SHL; EQUAL; e2 = expr { Assign(e1,Shl,e2)}
  | e1 = expr; SHR; EQUAL; e2 = expr { Assign(e1,Shr,e2)}
  | e = expr; SEMICOLON { Expr(e) }

  | b = block_stmt { b }

  | IF; LPAREN; cond = expr; RPAREN; then_branch = stmt; else_branch = option(preceded(ELSE, stmt))
    { If(cond, then_branch, else_branch) }

  | WHILE; LPAREN; cond = expr; RPAREN; body = stmt
    { While(cond, body) }

  | RETURN; e = option(expr); SEMICOLON
    { Return e }
    
  | DEFER; s = stmt
    { Defer s }
    
  | DEFER; s = block_stmt
    { Defer s }
  
  | DEFER; s = block_stmt; sem = SEMICOLON
    {  
        let msg = sprintf "Unnecessary semiclon after defer block" in 
        Diagnostic.add_warning msg $startpos(sem) ;
        Defer(s)
    }
  
  | SWITCH;  e = expr;  LBRACE; cases = list(switch_case); default = option(switch_default); RBRACE
    { Expr(Switch(e, cases, default)) } (* Wrapped in Expr for now, strictly it's an expr in our AST *)

block_stmt:
  | LBRACE; stmts = list(stmt); RBRACE { Block stmts }


switch_case:
  | vals = separated_nonempty_list(COMMA, expr); ARROW; s = block_or_stmt 
    { (vals, [s]) } (* /* AST expects stmt list for case body */ *)

switch_default:
  | ELSE; ARROW; s = block_or_stmt { [s] }

block_or_stmt:
  | s = stmt { s }
  (* /* If we allowed bare blocks without braces in switch, handle here */ *)

  (* | i = NUMBER_LITERAL { Int i } *)
  (* | x = IDENTIFIER  { Variable x } *)
ident_expr:
  | name = IDENTIFIER    { Identifier name } 
  | e = expr; DOT; field = IDENTIFIER   { Member(e, field) }


struct_init_field:
    | id = option(preceded(DOT,IDENTIFIER)); EQUAL; e = expr { (id, e) } 
  
  (* /* Binary Ops */ *)
  (* /* Function Call: foo(1, 2) */ *)
  (*  Access: obj.field or obj->field  *)
  (*  Casting: (int) x  *)
  (*  Sizeof  *)
expr:
  | LPAREN; e = expr; RPAREN { e }
  
  | i = NUMBER_LITERAL     { NumberLiteral i }
  
  | s = STRING_LITERAL  { StringLiteral s }
  
  | c = CHAR_LITERAL { CharLiteral c}
  
  | TRUE            { BoolLiteral true }
  
  | FALSE           { BoolLiteral false }
  
  | NULL            { Null }
  
  | name = IDENTIFIER    { Identifier name } 
  
  | e1 = expr; PLUS;  e2 = expr { BinOp(e1, Add, e2) }
  
  | e1 = expr; MINUS; e2 = expr { BinOp(e1, Sub, e2) }
  
  | e1 = expr; MUL; e2 = expr { BinOp(e1, Mul, e2) }
  
  | e1 = expr; DIV;   e2 = expr { BinOp(e1, Div, e2) }
  
  | e1 = expr; MOD;   e2 = expr { BinOp(e1, Mod, e2) }
  
  | e1 = expr; LESS_THAN;    e2 = expr { BinOp(e1, Lt, e2) }
  
  | e1 = expr; GREATER_THAN;    e2 = expr { BinOp(e1, Gt, e2) }
  
  | e1 = expr; SHL; e2 = expr { BinOp(e1,Shl,e2)}
  
  | e1 = expr; SHR; e2 = expr { BinOp(e1,Shr,e2)}
  
  | e1 = expr; AMPERSAND; e2 = expr { BinOp(e1,Bit_And,e2)}
  
  | e1 = expr; PIPE; e2 = expr { BinOp(e1,Bit_Or,e2)}
  
  | TILDE; e1 = expr  { UnaryOp(Bit_Not,e1)}
  
  | MINUS; e1 = expr;  { UnaryOp(Negation,e1)}
  
  | AMPERSAND; e1 = expr;  { UnaryOp(Address_Of,e1)}
  
  | MUL; e1 = expr;  { UnaryOp(Deref,e1)}
  
  | BANG; e1 = expr;  { UnaryOp(Bool_Not,e1)}

  | name = expr; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { Call(name, args) }
    (* A.B.C.D*)
  | e = expr; DOT; field = IDENTIFIER     
  { Member(e, field) }

  | LPAREN; t = typ; RPAREN; e = expr %prec TYPE_CAST
    { Cast(t, e) }
    
  | SIZEOF; LPAREN; t = typ; RPAREN { SizeOf t }

  | t = ident_expr; LBRACE ;
  init_l = separated_list(COMMA,struct_init_field) ;RBRACE ;
  SEMICOLON { StructInit(t,init_l)}
