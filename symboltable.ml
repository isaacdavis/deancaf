
open Globals

class ['a] symbol_table = object

  val mutable table : (string, 'a) Hashtbl.t = Hashtbl.create 0

  (* TODO the fail-on-repeated-put behavior is janky outside the context of the typechecker *)
  method put (k : string) (v : 'a) =
    if Hashtbl.mem table k then
      type_err_list := ("Previously declared: " ^ k) :: !type_err_list
    else
      Hashtbl.add table k v

  method get (k : string) : 'a = Hashtbl.find table k

  method get_opt (k : string) : 'a option =
    try
      Some (Hashtbl.find table k)
    with
      | Not_found -> None

  method contains (k : string) : bool =
    Hashtbl.mem table k

  method iter f =
    Hashtbl.iter f table

  method set_table t =
    table <- t

  method size = Hashtbl.length table

  method clone =
    let clone_table : 'a symbol_table = new symbol_table in
    let clone_hash = Hashtbl.copy table in
    clone_table#set_table clone_hash;
    clone_table

end

(* TODO make not crappy - roll my own stack? *)
class ['a] symbol_table_manager = object

  val stack : 'a symbol_table Stack.t = Stack.create ()

  method push (s : 'a symbol_table) =
    Stack.push s stack

  method pop =
    Stack.pop stack

  method top =
    Stack.top stack

  method lookup name : 'a option =
    let sl = ref [] in
    Stack.iter (fun x -> sl := x :: !sl) stack;

    let rec loop = function
      | [] -> None
      | h :: t ->
        (match h#get_opt (name) with
          | Some v -> Some v
          | None -> loop (t))
    in
    loop (!sl)

end


(* let walk_binop = function
  | Asgn
  | Greater
  | Less
  | Equals
  | Geq
  | Leq
  | Neq
  | BinPlus
  | BinMinus
  | Times
  | Div
  | And
  | Or
  | Mod

let walk_unop = function
  | UnPlus
  | UnMinus
  | Not

let walk_literal = function
  | NullLiteral
  | BoolLiteral of bool
  | IntLiteral of int
  | CharLiteral of char
  | StringLiteral of string

let walk_formal formal = 1

let walk_newarr newarr = 1

let walk_nonnew = function
  | LiteralExpr of astLiteral
  | ThisExpr
  | ParenExpr of astExpr
  | NewObjExpr of string * astExpr list
  | ThisCallExpr of string * astExpr list
  | MethodCallExpr of astPrimary * string * astExpr list
  | SuperCallExpr of string * astExpr list
  | ArrayExpr of astPrimary * astExpr
  | FieldExpr of astPrimary * string
  | SuperFieldExpr of string

let walk_expr = function
  | UnOpExpr of astUnOp * astExpr
  | BinOpExpr of astBinOp * astExpr * astExpr
  | PrimaryExpr of astPrimary

let walk_primary = function
  | NewArrayPrimary of astNewArrayExpr
  | NonNewArrayPrimary of astNonNewArrayExpr
  | IdPrimary of string

let walk_vardecl vardecl = 1

let walk_statement = function
  | EmptyStatement
  | DeclStatement of astType * astVarDecl list
  | IfStatement of astType symbol_table * astExpr * astStatement * astStatement option
  | ExprStatement of astExpr
  | WhileStatement of astType symbol_table * astExpr * astStatement
  | ReturnStatement of astExpr option
  | ContinueStatement
  | BreakStatement
  | BlockStatement of astType symbol_table * astStatement list
  | SuperStatement of astExpr list

let walk_member = function
  | Field of astModifier list * astType * astVarDecl
  | Method of string * astType * astType symbol_table * astModifier list * astFormal list * astStatement list
  | Constructor of string * astType * astModifier list * astFormal list * astStatement list

let walk_class c = 1   *)