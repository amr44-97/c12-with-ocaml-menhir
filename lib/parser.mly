%{
  open Ast
%}

/* Tokens (The vocabulary) */
%token <int> NUMBER_LITERAL
%token <string> IDENTIFIER
%token <string> CHAR_LITERAL 
%token <string> STRING_LITERAL 
%token <string> TYPE_NAME
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
  (*  *)
  (* | STRUCT; name = IDENTIFIER; LBRACE; fields = separated_list(COMMA,struct_field); option(COMMA);RBRACE  *)
  (*   { StructDef(name, fields) } *)
  
  | ENUM; name = IDENTIFIER; LBRACE; variants = separated_list(COMMA, IDENTIFIER); RBRACE 
    { EnumDef(name, variants) }

  | UNION; name = IDENTIFIER; enum_tag = option(IDENTIFIER) ;LBRACE; variants = separated_list(COMMA, IDENTIFIER); RBRACE 
    { UnionDef(name, enum_tag, variants) }

  | INCLUDE; path = IDENTIFIER ; alias = option(preceded(AS, IDENTIFIER)); SEMICOLON
    { Include(path, alias) }


(* /* --- Function Parameters --- */ *)
param:
  | t = typ; name = IDENTIFIER { { p_type = t; p_name = name } }

(* /* --- Struct Fields --- */ *)
struct_field:
  | t = typ; name = IDENTIFIER (* option(preceded(EQUAL,expr)) *) { (name, t) }

(*  --- Types ---  *)
typ:
  | INT    { TInt }
  | FLOAT  { TFloat }
  | DOUBLE { TDouble }
  | SHORT  { TShort }
  | LONG   { TLong }
  | CHAR   { TChar }
  | BOOL   { TBool }
  | VOID   { TVoid }
  (* | TYPE    { TType } /* Generic 'type T' */ *)
  | name = TYPE_NAME { TNamed name }
  (* /* Pointers: int* */ *)
  | t = typ; MUL { TPtr t }
  (* /* Optionals: int? */ *)
  | t = typ; QUESTION_MARK { TOptional t }
  (* /* Arrays: int[] or int[10..n] */ *)
  | t = typ; LBRACKET; size = option(NUMBER_LITERAL); RBRACKET { TArray(t, size) }
  (* /* Slice: int[0..] or int[0..n] */ *)
  | t = typ; LBRACKET; size = option(NUMBER_LITERAL); option(ELLIPSIS2); option(NUMBER_LITERAL) ; RBRACKET { TSlice(t, size) }


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
    { Decl(ty, name, init) }

  | LET; name = IDENTIFIER; init = preceded(EQUAL, expr); SEMICOLON
    { Let(name, init) }
  
  | CONST; name = IDENTIFIER; init = preceded(EQUAL, expr); SEMICOLON
    { Const(name, init) }
  
  | AUTO; name = IDENTIFIER; init = preceded(EQUAL, expr); SEMICOLON
    { Auto(name, init) }

  | e1 = expr; EQUAL; e2 = expr; SEMICOLON
    { Assign(e1, e2) }
  
  | e1 = expr; ADD_ASSIGN; e2 = expr { Assign_Add(e1,e2)}
  | e1 = expr; SUB_ASSIGN; e2 = expr { Assign_Sub(e1,e2)}
  | e1 = expr; MUL_ASSIGN; e2 = expr { Assign_Mul(e1,e2)}
  | e1 = expr; DIV_ASSIGN; e2 = expr { Assign_Div(e1,e2)}
  | e1 = expr; MOD_ASSIGN; e2 = expr { Assign_Mod(e1,e2)}
  | e1 = expr; OR_ASSIGN; e2 = expr { Assign_Or(e1,e2)}
  | e1 = expr; XOR_ASSIGN; e2 = expr { Assign_Xor(e1,e2)}
  | e1 = expr; SHL_ASSIGN; e2 = expr { Assign_Shl(e1,e2)}
  | e1 = expr; SHR_ASSIGN; e2 = expr { Assign_Shr(e1,e2)}
  | e = expr; SEMICOLON
    { Expr(e) }

  | b = block_stmt { b }

  | IF; LPAREN; cond = expr; RPAREN; then_branch = stmt; else_branch = option(preceded(ELSE, stmt))
    { If(cond, then_branch, else_branch) }

  | WHILE; LPAREN; cond = expr; RPAREN; body = stmt
    { While(cond, body) }

  | RETURN; e = option(expr); SEMICOLON
    { Return e }
    
  | DEFER; s = stmt
    { Defer s }
    
  | SWITCH; LPAREN; e = expr; RPAREN; LBRACE; cases = list(switch_case); default = option(switch_default); RBRACE
    { Expr(Switch(e, cases, default)) } /* Wrapped in Expr for now, strictly it's an expr in our AST */

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
expr:
  | LPAREN; e = expr; RPAREN { e }
  | i = NUMBER_LITERAL     { NumberLiteral i }
  | s = STRING_LITERAL  { StringLiteral s }
  | c = CHAR_LITERAL { CharLiteral c}
  | TRUE            { BoolLiteral true }
  | FALSE           { BoolLiteral false }
  | NULL            { Null }
  | name = IDENTIFIER       { Identifier name }

  (* /* Binary Ops */ *)
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
  (* /* Function Call: foo(1, 2) */ *)
  | name = IDENTIFIER; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { Call(name, args) }

  (* /* Access: obj.field or obj->field */ *)
  | e = expr; DOT; field = IDENTIFIER   { Member(e, field) }
  (* | e = expr; ARROW; field = IDENTIFIER { UnaryOp(Deref, Member(e, field)) } /* Sugar */ *)

  (* /* Casting: (int) x */ *)
  | LPAREN; t = typ; RPAREN; e = expr %prec TYPE_CAST
    { Cast(t, e) }
    
  (* /* Sizeof */ *)
  | SIZEOF; LPAREN; t = typ; RPAREN { SizeOf t }
