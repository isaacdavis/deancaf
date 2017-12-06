
%{

open Ast
open Globals
open Runtime
open Symboltable
open Typechecker

let get_super name =
    match class_table#get name with
        (* TODO move this to typechecker? *)
        | None -> failwith ("superclass not found: " ^ name)
        | Some c -> c

let make_class name super members =
    let field_table : astMember symbol_table = new symbol_table in
    let method_table : astMember symbol_table = new symbol_table in
    let ctor = ref (make_default_ctor name) in
    let ctor_found = ref false in

    let add_symbol s =
        match s with
            | Field (_, _, var_decl) -> field_table#put var_decl.name s 
            | Method (n, _, _, _, _, _) -> method_table#put n s
            | Constructor (_, _, _, _, _) ->
                if !ctor_found then
                    err_list := ("Duplicate constructor: " ^ strMember s) ::
                        !err_list
                else begin
                    ctor_found := true;
                    ctor := s
                end;
    in

    List.iter add_symbol members;

    let c = {t = ClassType(name); super = Some super; constructor = !ctor;
        fieldTable = field_table; methodTable = method_table} in
    class_table#put name c;
    c


let make_method name t mods formals statements =
    let var_table : astType symbol_table = new symbol_table in
    (* let add_symbol s =
        match s with
            | DeclStatement (dt, decls) ->
                List.iter (fun d -> var_table#put d.name dt) decls
            | _ -> ()
    in

    List.iter add_symbol statements;*)
    List.iter (fun (f : astFormal) -> var_table#put f.name f.t) formals;
    Method(name, t, var_table, mods, formals, statements)

let make_constructor t mods formals statements =
    let var_table : astType symbol_table = new symbol_table in
    (* let add_symbol s =
        match s with
            | DeclStatement (dt, decls) ->
                List.iter (fun d -> var_table#put d.name dt) decls
            | _ -> ()
    in

    List.iter add_symbol statements;*)
    List.iter (fun (f : astFormal) -> var_table#put f.name f.t) formals;
    Constructor(t, var_table, mods, formals, statements)

let make_while e s =
    (* let var_table = ref (new symbol_table) in
    (match s with
        | DeclStatement (dt, decls) ->
            List.iter (fun d -> !var_table#put d.name dt) decls
        | BlockStatement (btable, _) -> var_table := btable#clone 
        | _ -> ());
    WhileStatement(!var_table, e, s) *)
    WhileStatement (new symbol_table, e, s)

let make_if e st sfo =
    (*let true_table = ref (new symbol_table) in
    let false_table = ref (new symbol_table) in

    (match st with
        | DeclStatement (dt, decls) ->
            List.iter (fun d -> !true_table#put d.name dt) decls
        | BlockStatement (btable, _) -> true_table := btable#clone 
        | _ -> ());

    match sfo with
        | Some sf ->
            ((match sf with
                | DeclStatement (dt, decls) ->
                    List.iter (fun d -> !false_table#put d.name dt) decls
                | BlockStatement (btable, _) -> false_table := btable#clone 
                | _ -> ());
             IfStatement(!true_table, Some !false_table, e, st, sfo))
        | None -> IfStatement(!true_table, None, e, st, sfo)*)
    match sfo with
        | Some sf -> IfStatement(new symbol_table, Some new symbol_table, e, st,
             sfo)
        | None -> IfStatement(new symbol_table, None, e, st, sfo)

let make_block statements =
    (*let var_table : astType symbol_table = new symbol_table in
    let add_symbol s =
        match s with
            | DeclStatement (dt, decls) ->
                List.iter (fun d -> var_table#put d.name dt) decls
            | _ -> ();
    in

    List.iter add_symbol statements;
    BlockStatement(var_table, statements)*)
    BlockStatement(new symbol_table, statements)

%}

%token BREAK
%token CLASS
%token CONTINUE
%token ELSE
%token EXTENDS
%token IF
%token NEW
%token PRIVATE
%token PROTECTED
%token PUBLIC
%token RETURN
%token STATIC
%token SUPER
%token THIS
%token WHILE

%token <string> ID

%token <int> INT
%token <char> CHAR
%token <string> STRING
%token <bool> BOOL
%token NULL

%token BOOLTYPE
%token CHARTYPE
%token INTTYPE
%token VOIDTYPE
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token SEMICOLON
%token COMMA
%token DOT

%token ASGN
%token GREATER
%token LESS
%token NOT
%token EQUALS
%token GEQ
%token LEQ
%token NEQ
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token AND
%token OR
%token MOD 

%token EOF

%right ASGN
%left OR
%left AND
%left EQUALS NEQ
%nonassoc GEQ LEQ GREATER LESS
%left PLUS MINUS
%left MOD DIV TIMES
%nonassoc NOT UMINUS UPLUS

/*
    RE: LOWER_THAN_ELSE dangling else solution: see 
    https://stackoverflow.com/questions/1737460/how-to-find-shift-reduce-conflict-in-this-yacc-file
*/
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* TODO this is not elegant when combined with the class table - fix? */
%start classList
%type <Ast.astClass list> classList

%%

classList
    : singleClass classList                     { [$1]@$2 }
    | singleClass                               { [$1] }
    | EOF                                       { [] }
    ;

singleClass
    : CLASS ID LBRACE RBRACE                    { make_class $2 object_class [] }

    | CLASS ID super LBRACE RBRACE              { make_class $2 $3 [] }

    | CLASS ID LBRACE memberlist RBRACE         { make_class $2 object_class $4 }

    | CLASS ID super LBRACE memberlist RBRACE   { make_class $2 $3 $5 }
    ;

super
    : EXTENDS ID                                { get_super $2 }
    ;

memberlist
    : member memberlist                         { $1@$2 }
    | member                                    { $1 }
    ;

member
    : field                                     { $1 }
    | methodDecl                                { $1 }
    | ctor                                      { $1 }
    ;

field
    : modifierlist typeD varDeclList SEMICOLON  { List.map (fun d -> Field($1, $2, d)) $3 }
    | typeD varDeclList SEMICOLON               { List.map (fun d -> Field([Public], $1, d)) $2 }
    ;

modifierlist
    : modifier modifierlist                     { [$1]@$2 }
    | modifier                                  { [$1] }
    ;

methodDecl
    : modifierlist typeD ID formalArgs block    { [make_method $3 $2 $1 $4 $5] }
    | typeD ID formalArgs block    { [make_method $2 $1 [Public] $3 $4] }
    ;

ctor
    : modifierlist ID formalArgs block          { [make_constructor (ClassType($2)) $1 $3 $4] }
    | ID formalArgs block                       { [make_constructor (ClassType($1)) [Public] $2 $3]}
    ;

modifier
    : STATIC                                    { Static }
    | PUBLIC                                    { Public }
    | PRIVATE                                   { Private }
    | PROTECTED                                 { Protected }
    ;

formalArgs
    : LPAREN formalArgList RPAREN               { $2 }
    | LPAREN RPAREN                             { [] }
    ;

formalArgList
    : formalArg COMMA formalArgList             { [$1]@$3 }
    | formalArg                                 { [$1] }
    ;

formalArg
    : typeD varDeclId
    {
        let declName, declCnt = $2 in
        if declCnt != 0 then
            match $1 with
            | ClassType s ->
                {name = declName; t = ArrayType(ClassType(s), declCnt)}
            | ArrayType (arrayT, c) ->
                {name = declName; t = ArrayType(arrayT, c + declCnt)}
            | BoolType ->
                {name = declName; t = ArrayType(BoolType, declCnt)}
            | CharType ->
                {name = declName; t = ArrayType(CharType, declCnt)}
            | IntType ->
                {name = declName; t = ArrayType(IntType, declCnt)}
            (* TODO should a void type in a formal arg be a parse error? Or maybe offload to type checker *)
            | VoidType ->
                {name = declName; t = ArrayType(VoidType, declCnt)}
            (* TODO these aren't really parse errors *)
            | MethodType (_, _, _) -> raise Parsing.Parse_error
        else
            {name = declName; t = $1}
    }
    ;

typeD
    : primitiveType                             { $1 }
    | ID                                        { ClassType($1) }
    | primitiveType bracketList                 { ArrayType($1, $2) }
    | ID bracketList                            { ArrayType(ClassType($1), $2) }
    ;

bracketList
    : LBRACKET RBRACKET bracketList             { 1 + $3}
    | LBRACKET RBRACKET                         { 1 }
    ;

primitiveType
    : BOOLTYPE                                  { BoolType }
    | CHARTYPE                                  { CharType }
    | INTTYPE                                   { IntType }
    | VOIDTYPE                                  { VoidType }
    ;

varDeclList
    : varDecl COMMA varDeclList                 { [$1]@$3 }
    | varDecl                                   { [$1] }
    ;

varDecl
    : varDeclId ASGN expr                       { {name = fst $1; dim = snd $1; expr = Some $3} }
    | varDeclId                                 { {name = fst $1; dim = snd $1; expr = None} }
    ;

varDeclId
    : varDeclId LBRACKET RBRACKET               { fst $1, snd $1 + 1 }
    | ID                                        { $1, 0 }
    ;

block
    : LBRACE statementList RBRACE               { $2 }
    | LBRACE RBRACE                             { [] }
    ;

statementList
    : statement statementList                   { [$1]@$2 }
    | statement                                 { [$1] }
    ;

statement
    : SEMICOLON                                         { EmptyStatement }
    | typeD varDeclList SEMICOLON                       { DeclStatement($1, $2) }
    | IF LPAREN expr RPAREN statement
        %prec LOWER_THAN_ELSE                           { make_if $3 $5 None }
    | IF LPAREN expr RPAREN statement ELSE statement    { make_if $3 $5 (Some $7) }
    | expr SEMICOLON                                    { ExprStatement($1) }
    | WHILE LPAREN expr RPAREN statement                { WhileStatement(new symbol_table, $3, $5) }
    | RETURN SEMICOLON                                  { ReturnStatement(None) }
    | RETURN expr SEMICOLON                             { ReturnStatement(Some $2) }
    | CONTINUE SEMICOLON                                { ContinueStatement }
    | BREAK SEMICOLON                                   { BreakStatement }
    | block                                             { make_block $1 }
    | SUPER actualArgs SEMICOLON                        { SuperStatement($2) }
    ;

expr
    : expr ASGN expr                            { BinOpExpr(Asgn, $1, $3) }
    | expr GREATER expr                         { BinOpExpr(Greater, $1, $3) }
    | expr LESS expr                            { BinOpExpr(Less, $1, $3) }
    | expr EQUALS expr                          { BinOpExpr(Equals, $1, $3) }
    | expr GEQ expr                             { BinOpExpr(Geq, $1, $3) }
    | expr LEQ expr                             { BinOpExpr(Leq, $1, $3) }
    | expr NEQ expr                             { BinOpExpr(Neq, $1, $3) }
    | expr PLUS expr                            { BinOpExpr(BinPlus, $1, $3) }
    | expr MINUS expr                           { BinOpExpr(BinMinus, $1, $3) }
    | expr TIMES expr                           { BinOpExpr(Times, $1, $3) }
    | expr DIV expr                             { BinOpExpr(Div, $1, $3) }
    | expr AND expr                             { BinOpExpr(And, $1, $3) }
    | expr OR expr                              { BinOpExpr(Or, $1, $3) }
    | expr MOD expr                             { BinOpExpr(Mod, $1, $3) }
    | PLUS expr %prec UPLUS                     { UnOpExpr(UnPlus, $2) }
    | MINUS expr %prec UMINUS                   { UnOpExpr(UnMinus, $2) }
    | NOT expr                                  { UnOpExpr(Not, $2) }
    | primary                                   { PrimaryExpr($1) }
    ;

primary
    : newArrayExpr                              { NewArrayPrimary($1) }
    | nonNewArrayExpr                           { NonNewArrayPrimary($1) }
    | ID                                        { IdPrimary($1) }
    ;

newArrayExpr
    : NEW ID dimensionList                      { {t = ClassType($2); dimList = $3} }
    | NEW primitiveType dimensionList           { {t = $2; dimList = $3}}
    ;

dimensionList
    : dimension dimensionList                   { [$1]@$2 }
    | dimension                                 { [$1] }
    ;

dimension
    : LBRACKET expr RBRACKET                    { $2 }
    ;

nonNewArrayExpr
    : literal                                   { LiteralExpr($1) }
    | THIS                                      { ThisExpr }
    | LPAREN expr RPAREN                        { ParenExpr($2) }
    | NEW ID actualArgs                         { NewObjExpr($2, $3) }
    | ID actualArgs                             { ThisCallExpr($1, $2) }
    | primary DOT ID actualArgs                 { MethodCallExpr($1, $3, $4) }
    | SUPER DOT ID actualArgs                   { SuperCallExpr($3, $4) }
    | arrayExpr                                 { $1 }
    | fieldExpr                                 { $1 }
    ;

fieldExpr
    : primary DOT ID                            { FieldExpr($1, $3)}
    | SUPER DOT ID                              { SuperFieldExpr($3) }
    ;

arrayExpr
    : ID dimension                              { ArrayExpr(IdPrimary($1), $2) }
    | nonNewArrayExpr dimension                 { ArrayExpr(NonNewArrayPrimary($1), $2) }
    ;

literal
    : NULL                                      { NullLiteral }
    | BOOL                                      { BoolLiteral($1) }
    | INT                                       { IntLiteral($1) }
    | CHAR                                      { CharLiteral($1) }
    | STRING                                    { StringLiteral($1) }
    ;

actualArgs
    : LPAREN exprList RPAREN                    { $2 }
    | LPAREN RPAREN                             { [] }
    ;

exprList
    : expr COMMA exprList                       { [$1]@$3 }
    | expr                                      { [$1] }
    ;

%%
