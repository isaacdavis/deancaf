
%{

open List
open Ast

let objectClass = {super = ClassType("Object")} 

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

/* TODO this is janky janky */
%nonassoc LOWER_THAN_LBRACKET 

%start classList
%type <Ast.astClass list> classList

%%

classList
	: singleClass classList                     { [$1]@$2 }
	| singleClass                               { [$1] }
	| EOF                                       { [] }
	;

singleClass
	: CLASS ID LBRACE RBRACE                    { {name = $2; super = objectClass; memberList = []} }
	| CLASS ID super LBRACE RBRACE              { {name = $2; super = $3; memberList = []} }
	| CLASS ID LBRACE memberlist RBRACE         { {name = $2; super = objectClass; memberList = $4} }
	| CLASS ID super LBRACE memberlist RBRACE   { {name = $2; super = $3; memberList = $5} }
	;

super
	:EXTENDS ID                                 { {super = ClassType($2)} }
	;

memberlist
	: member memberlist                         { [$1]@$2 }
	| member                                    { [$1] }
	;

member
	: field                                     { $1 }
	| methodDecl                                { $1 }
	| ctor                                      { $1 }
	;

/* TODO condense field lists into one list - do this after parsing is done? List.map (fun varDecl -> Field($1, $2, varDecl)) $3  */
field
	: modifierlist typeD varDeclList SEMICOLON
	{
		Field($1, $2, $3)
	}
	;

modifierlist
	: modifier modifierlist                     { [$1]@$2 }
	| modifier                                  { [$1] }
	;

methodDecl
	: modifierlist typeD ID formalArgs block        { Method($3, $2, $1, $4, $5) }
	;

ctor
	: modifierlist ID formalArgs block          { Constructor($2, ClassType($2), $1, $3, $4) }
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
	: typeD varDeclId                           { {name = $2; t = $1} }
	;

typeD
	: primitiveType                             { $1 }
	| ID                                        { ClassType($1) }
	| typeD LBRACKET RBRACKET                   { ArrayType($1) }
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

/* TODO fix fix fix fix */
varDecl
	: varDeclId ASGN expr                       { {name = $1; expr = Some $3} }
	| varDeclId                                 { {name = $1; expr = None} }
	;

/* TODO fix fix fix fix */
varDeclId
	: varDeclId LBRACKET RBRACKET               { $1 }
	| ID                                        { $1 }
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
		%prec LOWER_THAN_ELSE                           { IfStatement($3, $5, None) }
	| IF LPAREN expr RPAREN statement ELSE statement    { IfStatement($3, $5, Some $7) }
	| expr SEMICOLON                                    { ExprStatement($1) }
	| WHILE LPAREN expr RPAREN statement                { WhileStatement($3, $5) }
	| RETURN SEMICOLON                                  { ReturnStatement(None) }
	| RETURN expr SEMICOLON                             { ReturnStatement(Some $2) }
	| CONTINUE SEMICOLON                                { ContinueStatement }
	| BREAK SEMICOLON                                   { BreakStatement }
	| block                                             { BlockStatement($1) }
	| SUPER actualArgs SEMICOLON                        { SuperStatement($2) }
	;

/* TODO is prec UMINUS here ok? */
expr
	: expr binOp expr                           { BinOpExpr($2, $1, $3) }
	| unOp expr %prec UMINUS                    { UnOpExpr($1, $2) }
	| primary                                   { PrimaryExpr($1) }
	;

binOp
	: ASGN                                      { Asgn }            
	| GREATER                                   { Greater }
	| LESS                                      { Less }
	| EQUALS                                    { Equals }
	| GEQ                                       { Geq }
	| LEQ                                       { Leq }
	| NEQ                                       { Neq }
	| PLUS                                      { BinPlus }
	| MINUS                                     { BinMinus }
	| TIMES                                     { Times }
	| DIV                                       { Div }
	| AND                                       { And }
	| OR                                        { Or }
	| MOD                                       { Mod }
	;

unOp
	: PLUS        %prec UPLUS                   { UnPlus }
	| MINUS       %prec UMINUS                  { UnMinus }
	| NOT                                       { Not }
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
