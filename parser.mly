%{
    open Absyn
    module Pos = Position

    let pos () =
      Pos.of_lexing_positions
        ~pos_start:(Parsing.symbol_start_pos ())
        ~pos_end:(Parsing.symbol_end_pos ())

    let add_to_typedec new_type = function
      | [] -> TypeDec ([new_type]) :: []
      | TypeDec (a) :: l -> TypeDec (new_type :: a) :: l
      | a :: l -> TypeDec ([])  :: []

    let add_to_fundec new_fun = function
      | [] -> FunctionDec ([new_fun]) :: []
      | FunctionDec (a) :: l -> FunctionDec (new_fun :: a) :: l
      | a :: l -> FunctionDec ([]) :: []

%}

%token TYPE
%token VAR
%token FUNCTION
%token NIL
%token BREAK
%token OF
%token END
%token LET
%token IN
%token ARRAY
%token DO
%token TO
%token FOR
%token WHILE
%token IF
%token THEN
%token ELSE
%token OR
%token AND
%token GE
%token GT
%token LE
%token LT
%token NEQ
%token EQ
%token DIVIDE
%token TIMES
%token PLUS
%token MINUS
%token DOT
%token ASSIGN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token SEMICOLON
%token COLON
%token COMMA

%token <string> STRING
%token <int> INT
%token <string> ID

%token EOF

/* from lowest precedence */
%nonassoc THEN
%nonassoc ELSE
%nonassoc ASSIGN
%nonassoc OF DO
%left OR
%left AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc HIGHEST
/* to highest precedence */

%start main
%type <Absyn.t> main

%%

main :
  exp EOF { $1 }
;

exp : lvalue
        { VarExp ($1) }
    |  NIL
        { NilExp }
    | LPAREN exp SEMICOLON exp exptail RPAREN
        {
          let pos = pos () in
          SeqExp (($2, pos) :: ($4, pos) :: $5)
        }
    | LPAREN RPAREN
        { SeqExp ([]) }
    | LET decs IN exps END
        { LetExp { decs = $2; body = SeqExp ($4); pos = pos ()} }
    | INT
        { IntExp ($1) }
    | STRING
        { StringExp {string = $1; pos = pos ()}}
    | ID LPAREN funarg RPAREN
        { CallExp {func = Symbol.symbol $1; args = $3; pos = pos ()} }
    | MINUS exp %prec HIGHEST
        { OpExp {left = IntExp (0); oper = MinusOp; right = $2; pos = pos ()} }
    | exp PLUS exp
        { OpExp {left = $1; oper = PlusOp; right = $3; pos = pos ()} }
    | exp MINUS exp
        { OpExp {left = $1; oper = MinusOp; right = $3; pos = pos ()} }
    | exp TIMES exp
        { OpExp {left = $1; oper = TimesOp; right = $3; pos = pos ()} }
    | exp DIVIDE exp
        { OpExp {left = $1; oper = DivideOp; right = $3; pos = pos ()} }
    | exp EQ exp
        { OpExp {left = $1; oper = EqOp; right = $3; pos = pos ()} }
    | exp NEQ exp
        { OpExp {left = $1; oper = NeqOp; right = $3; pos = pos ()} }
    | exp LT exp
        { OpExp {left = $1; oper = LtOp; right = $3; pos = pos ()} }
    | exp LE exp
        { OpExp {left = $1; oper = LeOp; right = $3; pos = pos ()} }
    | exp GT exp
        { OpExp {left = $1; oper = GtOp; right = $3; pos = pos ()} }
    | exp GE exp
        { OpExp {left = $1; oper = GeOp; right = $3; pos = pos ()} }
    | exp AND exp
        { IfExp {test = $1; then' = $3; else' = Some (IntExp (0)); pos = pos ()}  }
    | exp OR exp
        { IfExp {test = $1; then' = IntExp (1); else' = Some ($3); pos = pos ()}  }
    | lvalue ASSIGN exp
        { AssignExp {var = $1; exp = $3; pos = pos ()} }
    | ID LBRACE recordbody RBRACE
        { RecordExp {fields = $3; typ = Symbol.symbol $1; pos = pos ()} }
    | ID LBRACK exp RBRACK OF exp
        { ArrayExp {typ = Symbol.symbol $1; size = $3; init = $6; pos = pos ()} }
    | IF exp THEN exp ELSE exp
        { IfExp {test = $2; then' = $4; else' = Some ($6); pos = pos ()} }
    | IF exp THEN exp
        { IfExp {test = $2; then' = $4; else' = None; pos = pos ()} }
    | WHILE exp DO exp
        { WhileExp {test = $2; body = $4; pos = pos ()} }
    | FOR ID ASSIGN exp TO exp DO exp
        { ForExp {var = Symbol.symbol $2; escape = ref true; lo = $4; hi = $6; body = $8; pos = pos ()} }
    | BREAK
        { BreakExp (pos ()) }
    | lvalue LBRACK exp RBRACK OF exp
        {
          match $1 with
          | SimpleVar {symbol=typ; _} ->
             ArrayExp { typ; size = $3; init = $6; pos = pos () }
          | _ -> raise Parse_error
        }
;

recordbody : ID EQ exp                  { (Sym.symbol $1, $3, pos()) :: [] }
           | ID EQ exp COMMA recordbody { (Sym.symbol $1, $3, pos()) :: $5 }
           |                            { [] }

lvalue :
  | ID
      { SimpleVar {symbol = Sym.symbol $1; pos = pos ()} }
  | lvalue DOT ID
      { FieldVar { var = $1; symbol = Sym.symbol $3; pos = pos ()} }
  | ID LBRACK exp RBRACK
      {
        let pos = pos () in
        let var = SimpleVar {symbol = Sym.symbol $1; pos } in
        SubScriptVar {var = var; exp = $3; pos = pos}
      }
  | lvalue LBRACK exp RBRACK
      { SubScriptVar {var = $1; exp = $3; pos = pos ()} }
;

letbody :
    exp exptail { SeqExp (($1, pos ()) :: $2) }
  |             { NilExp }
;

exptail :
    SEMICOLON exp exptail     { ($2, pos()) :: $3 }
  |                           { [] }
;

exps :
    exp                       { ($1, pos ()) :: [] }
  | exp SEMICOLON exps        { ($1, pos ()) :: $3 }
  |                           { [] }
;

decs : tydec nontydec         { $1 @ $2 }
     | vardec decs            { $1 :: $2 }
     | fundec nonfundec       { $1 @ $2 }
     |                        { [] }
;

nontydec :
    vardec decs               { $1 :: $2  }
  | fundec nonfundec          { $1 @ $2 }
  |                           { [] }
;

nonfundec : vardec decs       { $1 :: $2 }
          | tydec nontydec    { $1 @ $2 }
          |                   { [] }
;


tydec : TYPE ID EQ ty   { TypeDec ([{name = Sym.symbol $2; ty = $4; pos = pos () }]) :: [] }
      | TYPE ID EQ ty tydec { add_to_typedec {name = Sym.symbol $2; ty = $4; pos = pos ()} $5 }


ty : ID                     { NameTy { symbol = Symbol.symbol $1; pos = pos () } }
   | LBRACE tyfields RBRACE { RecordTy ($2) }
   | ARRAY OF ID            { ArrayTy { symbol = Symbol.symbol $3; pos = pos () } }

tyfields :
    ID COLON ID
    {
      let field =
        Field { name = Sym.symbol $1; escape = ref true; typ = Sym.symbol $3; pos = pos () }
      in
      field :: []
    }
  | ID COLON ID COMMA tyfields
      {
        let field =
          Field { name = Sym.symbol $1; escape = ref true; typ = Sym.symbol $3; pos = pos () }
        in
        field :: $5
      }
  | { [] }
;

vardec :
    VAR ID ASSIGN exp
      { VarDec { name = Sym.symbol $2; escape = ref true; typ = None; init = $4; pos = pos() } }
  | VAR ID COLON ID ASSIGN exp
      {
        let pos = pos () in
        let typ = Some (Symbol.symbol $4, pos) in
        VarDec { name = Sym.symbol $2; escape = ref true; typ = typ; init = $6; pos = pos }
      }
;

fundec : FUNCTION ID LPAREN tyfields RPAREN EQ exp
           {
             let new_fun = {name = Sym.symbol $2; params = $4; result = None; body = $7; pos = pos ()} in
             FunctionDec ([new_fun]) :: []
           }
       | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp
           {
             let pos = pos () in
             let new_fun = {name = Sym.symbol $2; params = $4; result = Some (Sym.symbol $7, pos); body = $9; pos = pos} in
             FunctionDec ([new_fun]) :: []
           }
       | FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec
           {
             let new_fun = {name = Sym.symbol $2; params = $4; result = None; body = $7; pos = pos ()} in
             let fundecs = $8 in
             add_to_fundec new_fun fundecs
           }
       | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec
           {
             let pos = pos () in
             let new_fun = {name = Sym.symbol $2; params = $4; result = Some (Sym.symbol $7, pos); body = $9; pos = pos} in
             let fundecs = $10 in
             add_to_fundec new_fun fundecs
           }
;

funarg :                     { [] }
       | exp funargtail      { $1 :: $2 }
;

funargtail : COMMA exp funargtail  { $2 :: $3  }
           |                       { [] }
;
