%{
    open Ast
%}

%token <int> INT
%token <string> ID STRING
%token WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY
%token IF THEN ELSE DO OF NIL
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token DOT PLUS MINUS ASTERISK SLASH
%token EQUAL NEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
%token AND OR ASSIGN
%token EOF

%nonassoc IF_THEN
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQUAL NEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc UMINUS

%start <Ast.expr> program

%%

program:
  | e = expr; EOF { e }

expr:
  | i = INT                                                         { Int i }
  | s = STRING                                                      { String s }
  | NIL                                                             { Nil }
  | BREAK                                                           { Break }
  | lv = lvalue                                                     { LValue lv }
  | MINUS; e = expr %prec UMINUS                                    { UnaryMinus e }
  | e1 = expr; PLUS;         e2 = expr                              { BinOp (e1, Add,  e2) }
  | e1 = expr; MINUS;        e2 = expr                              { BinOp (e1, Sub,  e2) }
  | e1 = expr; ASTERISK;     e2 = expr                              { BinOp (e1, Mult, e2) }
  | e1 = expr; SLASH;        e2 = expr                              { BinOp (e1, Div,  e2) }
  | e1 = expr; EQUAL;        e2 = expr                              { BinOp (e1, Eq,   e2) }
  | e1 = expr; NEQUAL;       e2 = expr                              { BinOp (e1, Neq,  e2) }
  | e1 = expr; LESS;         e2 = expr                              { BinOp (e1, Lt,   e2) }
  | e1 = expr; LESSEQUAL;    e2 = expr                              { BinOp (e1, Le,   e2) }
  | e1 = expr; GREATER;      e2 = expr                              { BinOp (e1, Gt,   e2) }
  | e1 = expr; GREATEREQUAL; e2 = expr                              { BinOp (e1, Ge,   e2) }
  | e1 = expr; AND;          e2 = expr                              { IfThenElse (e1, e2, Int 0) }
  | e1 = expr; OR;           e2 = expr                              { IfThenElse (e1, Int 1, e2) }
  | lv = lvalue; ASSIGN; e = expr                                   { Assign (lv, e) }
  | x = ID; LPAREN; args = separated_list(COMMA, expr); RPAREN     { Call (x, args) }
  | LPAREN; es = separated_list(SEMICOLON, expr); RPAREN            { Seq es }
  | IF; cond = expr; THEN; e = expr %prec IF_THEN                   { IfThen (cond, e) }
  | IF; cond = expr; THEN; e1 = expr; ELSE; e2 = expr               { IfThenElse (cond, e1, e2) }
  | WHILE; cond = expr; DO; body = expr                             { While (cond, body) }
  | FOR; x = ID; ASSIGN; lo = expr; TO; hi = expr; DO; body = expr { For (x, lo, hi, body) }
  | LET; ds = list(decl); IN; es = separated_list(SEMICOLON, expr); END { Let (ds, es) }
  | x = ID; LBRACE; fields = separated_list(COMMA, record_field); RBRACE { RecordCreate (x, fields) }
  | lv = lvalue; LBRACKET; size = expr; RBRACKET; OF; init = expr   {
      match lv with
      | LId x -> ArrayCreate (x, size, init)
      | _ -> failwith "array creation requires a type name"
    }

lvalue:
  | x = ID                                   { LId x }
  | lv = lvalue; DOT; x = ID                 { LField (lv, x) }
  | lv = lvalue; LBRACKET; e = expr; RBRACKET { LIndex (lv, e) }

record_field:
  | x = ID; EQUAL; e = expr { (x, e) }

decl:
  | TYPE; x = ID; EQUAL; t = ty                                                          { TyDec (x, t) }
  | VAR; x = ID; ASSIGN; e = expr                                                        { VarDec { var_name = x; var_ty = None; var_exp = e } }
  | VAR; x = ID; COLON; t = ID; ASSIGN; e = expr                                        { VarDec { var_name = x; var_ty = Some (TyId t); var_exp = e } }
  | FUNCTION; x = ID; LPAREN; ps = separated_list(COMMA, tyfield); RPAREN; EQUAL; body = expr
      { FunDec (x, ps, None, body) }
  | FUNCTION; x = ID; LPAREN; ps = separated_list(COMMA, tyfield); RPAREN; COLON; ret = ID; EQUAL; body = expr
      { FunDec (x, ps, Some (TyId ret), body) }

tyfield:
  | x = ID; COLON; t = ID { { field_name = x; field_ty = TyId t } }

ty:
  | x = ID                                                      { TyId x }
  | LBRACE; fields = separated_list(COMMA, tyfield); RBRACE     { TyRecord fields }
  | ARRAY; OF; t = ty                                           { TyArray t }
