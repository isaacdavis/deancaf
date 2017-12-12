
open Ast
open Icode
open Symboltable

let class_record_table : classRecord symbol_table = new symbol_table
let method_frame_table : icMethod symbol_table = new symbol_table
let string_literal_table : string symbol_table = new symbol_table

let offset_mgr : int symbol_table_manager = new symbol_table_manager

(* Relative to start of class object *)
let curr_field_offset = ref 0

(* Relative to start of vtable *)
let curr_method_offset = ref 0

(* Relative to ebp within function call *)
let min_local_offset = ref (-data_sz)
let curr_local_offset = ref (-data_sz)
let curr_param_offset = ref (2 * data_sz)


let string_id = ref 0
let alloc_string s =
  let name = "_S$" ^ (string_of_int !string_id) in
  string_literal_table#put name s;
  string_id := !string_id + 1;
  VerbatimVal(name)

let alloc_local_space () =
  let off = !curr_local_offset in
  curr_local_offset := !curr_local_offset - data_sz;
  min_local_offset := min !curr_local_offset !min_local_offset;
  off

let reset_local_space () =
  min_local_offset := -data_sz;
  curr_local_offset := -data_sz;
  curr_param_offset := 2 * data_sz

(* let get_type_size = function
  | ClassType _ -> ptr_sz
  | ArrayType (_, _) -> ptr_sz
  | BoolType -> bool_sz
  | CharType -> char_sz
  | IntType -> int_sz
  | VoidType -> failwith "get_type_size called on VoidType"
  | MethodType (_, _, _) -> ptr_sz (* TODO should this be a failwith? *) *)

let new_temporary () =
  let result = LocalVal(alloc_local_space ()) in
  result

let get_container c_o =
  match c_o with
  | Some c -> c
  | None -> new_temporary ()

let convert_unop = function
  | UnPlus -> Pos
  | UnMinus -> Neg
  | Not -> Not 

let convert_binop = function
  | Asgn -> Move
  | BinPlus -> Add
  | BinMinus -> Sub
  | Times -> Mult
  | Div -> Div
  | And -> And
  | Or -> Or
  | Mod -> Mod
  | _ -> failwith "convert_binop called on cond"

let convert_cond = function
  | Ast.Greater -> Icode.Greater
  | Less -> Less
  | Equals -> Equals
  | Geq -> Geq
  | Leq -> Leq
  | Neq -> Neq
  | _ -> failwith "convert_cond called on binop"
 
(* TODO factor stuff out into this method from walk_member
let add_formals_to_table m formals = *)

(* TODO it is horribly naive to put every literal in a memory address *)
let walk_literal m container l =
  let cont = get_container container in
  match l with
  | Ast.NullLiteral ->
    m.statements <- BinStatement(Move, LiteralVal(NullLiteral), cont) ::
      m.statements;
    cont
  | BoolLiteral b ->
    m.statements <- BinStatement(Move, LiteralVal(BoolLiteral(b)), cont) ::
      m.statements;
    cont
  | IntLiteral i ->
    m.statements <- BinStatement(Move, LiteralVal(IntLiteral(i)), cont) ::
      m.statements;
    cont
  | CharLiteral c ->
    m.statements <- BinStatement(Move, LiteralVal(CharLiteral(c)), cont) ::
      m.statements;
    cont
  | StringLiteral s ->
    let args = [alloc_string s] in
    m.statements <-
      NewObjStatement (cont, class_record_table#get "String", args) ::
      m.statements;
    cont

let rec walk_newarr m container newarr =
  let cont = get_container container in
  let dim_loc_list = List.map (walk_expr m None) newarr.dimList in
  m.statements <-
    NewArrayStatement(cont, newarr.t, dim_loc_list) :: m.statements;
  cont

and walk_nonnew m container nn =
  match nn with
  | LiteralExpr literal -> walk_literal m container literal

  | ThisExpr -> LocalVal(m.local_offset_table#get "this")

  | ParenExpr e -> walk_expr m container e

  | NewObjExpr (name, args) ->
    let cont = get_container container in
    let arg_locs = List.map (walk_expr m None) args in
    m.statements <-
      NewObjStatement(cont, class_record_table#get name, arg_locs) ::
      m.statements;
    cont

  | ThisCallExpr (name, args) ->
    let cont = get_container container in
    let arg_locs = List.map (walk_expr m None) args in
    let offset = m.c.method_offset_table#get name in
    let this_val = LocalVal(m.local_offset_table#get "this") in
    m.statements <-
      MethodCallStatement (cont, this_val, offset, arg_locs) ::
      m.statements;
    cont

  | MethodCallExpr (calleeprimary, name, args) ->
    let cont = get_container container in
    let arg_locs = List.map (walk_expr m None) args in

    let cr = match get_primary_type calleeprimary with
      | ClassType s -> class_record_table#get s
      | _ -> failwith "class does not have classtype"
    in

    let mangled_name = mangle_name cr.name name false in
    let static = (method_frame_table#get mangled_name).static in

    let method_statement = if static then
      StaticMethodCallStatement(cont, VerbatimVal(mangled_name), arg_locs)
    else begin
      let callee = walk_primary m None calleeprimary in
      let offset = cr.method_offset_table#get name in
      MethodCallStatement(cont, callee, offset, arg_locs)
    end in

    m.statements <- method_statement :: m.statements;
    cont

  | SuperCallExpr (name, args) ->
    (* TODO this is the same as "this" - is that ok? *)
    let cont = get_container container in
    let arg_locs = List.map (walk_expr m None) args in
    let offset = m.c.method_offset_table#get name in
    let this_val = LocalVal(m.local_offset_table#get "this") in
    m.statements <-
      MethodCallStatement(cont, this_val, offset, arg_locs) ::
      m.statements;
    cont

  | ArrayExpr (primary, indexexpr) ->
    let cont = get_container container in
    let ind = walk_expr m None indexexpr in
    let arr = walk_primary m None primary in
    m.statements <-
      ArrayStatement(cont, arr, ind) :: m.statements;
    cont

  | FieldExpr (primary, name) ->
    let cont = get_container container in
    let callee = walk_primary m None primary in

    let cr = match get_primary_type primary with
      | ClassType s -> class_record_table#get s
      | _ -> failwith "class does not have classtype"
    in

    let offset = cr.field_offset_table#get name in
    m.statements <-
      FieldAccessStatement(cont, callee, offset) ::
      m.statements;
    cont

  | SuperFieldExpr name ->
    (* TODO this is the same as "this" - is that ok? *)
    let cont = get_container container in
    let offset = m.c.field_offset_table#get name in
    let this_val = LocalVal(m.local_offset_table#get "this") in
    m.statements <-
      FieldAccessStatement(cont, this_val, offset) ::
      m.statements;
    cont

and walk_primary m container p =
  match p with
  | NewArrayPrimary (_, newarr) -> walk_newarr m container newarr
  | NonNewArrayPrimary (_, nonnew) -> walk_nonnew m container nonnew
  | IdPrimary (_, id) -> LocalVal(offset_mgr#lookup id)

and get_primary_type = function
  | NewArrayPrimary (tb, _) -> tb.t
  | NonNewArrayPrimary (tb, _) -> tb.t
  | IdPrimary (tb, _) -> tb.t

(* TODO handle offsets with temporaries *)
and walk_expr m container e =
  match e with
  | UnOpExpr (op, sube) ->
    let r_container = walk_expr m container sube in
    m.statements <- UnStatement(convert_unop op, r_container) :: m.statements;
    r_container
  
  | BinOpExpr (op, e1, e2) ->
    let r1_container = walk_expr m container e1 in
    let r2_container = walk_expr m None e2 in
    m.statements <-
      (* Order of exprs switched because this is how asm does it *)
      BinStatement(convert_binop op, r2_container, r1_container) ::
      m.statements;
    r1_container

  | PrimaryExpr primary -> walk_primary m container primary

let walk_vardecl m t decl =
  match decl.expr with
  | None -> ()
  | Some expr ->
    let cont = walk_expr m (Some (IdVal(decl.name))) expr in
    (match cont with
    | IdVal n -> if (String.compare n decl.name) != 0 then
      m.statements <-
        BinStatement(Move, cont, IdVal(decl.name)) ::
        m.statements
    | _ ->
      m.statements <-
        BinStatement(Move, cont, IdVal(decl.name)) ::
        m.statements)

let rec walk_statement m statement =

  let starting_offset = !curr_local_offset in

  (match statement with
  | EmptyStatement -> ()
  | DeclStatement (t, decls) ->

    List.iter (walk_vardecl m t) decls

  | IfStatement (then_tbl, else_tbl_o, e, then_statement, else_statement_o) -> ()
   (*  let ic_then_tbl = new symbol_table in
    let add tbl k v =
        tbl#put k !curr_local_offset;
        curr_local_offset := !curr_local_offset - get_type_size v
    in

    then_tbl#iter add (ic_then_tbl);



    match else_statement_o with
    | None ->

    (match else_tbl_o with
    | None -> ()
    | Some else_tbl -> else_tbl#iter add); *)



  | ExprStatement e ->
    ignore(walk_expr m None e)

  | WhileStatement (tbl, e, statement) -> ()
    (* let ic_tbl = new symbol_table in
    let add tbl k v =
        tbl#put k !curr_local_offset;
        curr_local_offset := !curr_local_offset - data_sz
    in
    tbl#iter add (ic_tbl); *)




  | ReturnStatement e_o ->
    (match e_o with
    | None -> m.statements <- ReturnStatement(None) :: m.statements
    | Some e ->
      let cont = walk_expr m None e in
      m.statements <- ReturnStatement(Some cont) :: m.statements)

  | ContinueStatement -> m.statements <- ContinueStatement :: m.statements
  | BreakStatement -> m.statements <- BreakStatement :: m.statements

  | BlockStatement (tbl, statements) ->
    List.iter (walk_statement m) statements

  | SuperStatement args ->
    let arg_loc_list = List.map (walk_expr m None) args in
    m.statements <- SuperStatement(arg_loc_list) :: m.statements);

  curr_local_offset := starting_offset

let walk_member cr cname member =
  match member with
  | Field (_, t, decl) ->
    if not (cr.field_offset_table#contains decl.name) then
    begin
      cr.field_offset_table#put decl.name !curr_field_offset;
      curr_field_offset := !curr_field_offset + data_sz
    end
  | Method (mname, _, tbl, mods, formals, _) ->
    if not (List.mem Static mods) &&
      not (cr.method_offset_table#contains mname) then
    begin
      cr.method_offset_table#put mname !curr_method_offset;
      curr_method_offset := !curr_method_offset + data_sz
    end;

    let mangled_name = mangle_name cname mname false in
    let m =
      { name = mangled_name
      ; c = cr
      ; size = 0
      ; local_offset_table = new symbol_table
      ; statements = []
      ; static = List.mem Static mods
      } in

    reset_local_space ();
    if not (List.mem Static mods) then begin
      m.local_offset_table#put "this" !curr_param_offset;
      curr_param_offset := !curr_param_offset + data_sz
    end;
    let formal_names = List.map (fun (f : astFormal) -> f.name) formals in
    let filter_and_add k v =
      if  List.mem k formal_names then
      begin
        m.local_offset_table#put k !curr_param_offset;
        curr_param_offset := !curr_param_offset + data_sz
      end
      else begin
        m.local_offset_table#put k (alloc_local_space ());
      end
    in
    tbl#iter filter_and_add;
    m.size <- -(!min_local_offset);
    method_frame_table#put mangled_name m

  | Constructor (_, tbl, _, formals, _) ->
    let mangled_name = mangle_name cname "" true in
    let m =
      { name = mangled_name
      ; c = cr
      ; size = 0
      ; local_offset_table = new symbol_table
      ; statements = []
      ; static = false
      } in

    reset_local_space ();
    (* Space for hidden "this" parameter *)
    m.local_offset_table#put "this" !curr_param_offset;
    curr_param_offset := !curr_param_offset + data_sz;
    let formal_names = List.map (fun (f : astFormal) -> f.name) formals in
    let filter_and_add k v =
      if  List.mem k formal_names then
      begin
        m.local_offset_table#put k !curr_param_offset;
        curr_param_offset := !curr_param_offset + data_sz
      end
      else begin
        m.local_offset_table#put k (alloc_local_space ());
      end
    in
    tbl#iter filter_and_add;
    m.size <- -(!min_local_offset);
    method_frame_table#put mangled_name m

let add_super_members c =
  match c.super with
  | None -> ()
  | Some super ->
    let add_to_tbl tbl name offset =
      tbl#put name offset
    in
    let add_to_field_table = add_to_tbl c.field_offset_table in
    let add_to_method_table = add_to_tbl c.method_offset_table in

    super.field_offset_table#iter add_to_field_table;
    super.method_offset_table#iter add_to_method_table;
    curr_field_offset := super.size - data_sz;
    curr_method_offset := super.method_offset_table#size * data_sz

let walk_class_offsets c =
  curr_field_offset := 0;
  curr_method_offset := 0;
  match c.t with
  | ClassType name ->
    let super =
      (match c.super with
      | None -> None
      | Some superc ->
        (match superc.t with
        | ClassType supername -> class_record_table#get_opt supername
        | _ -> failwith "class does not have ClassType")) in
    let cr = 
      { name = name
      ; super = super
      ; size = 0
      ; field_offset_table = new symbol_table
      ; method_offset_table = new symbol_table
      } in
    let walk_member_iter n b = walk_member cr name b in
    add_super_members cr;
    c.fieldTable#iter walk_member_iter;
    c.methodTable#iter walk_member_iter;
    walk_member cr name c.constructor;
    (* space for vtable and super ptr as well *)
    cr.size <- !curr_field_offset + (2 * data_sz);
    class_record_table#put name cr

  | _ -> failwith "class does not have ClassType"

let gen_offsets classlist =
  List.iter walk_class_offsets classlist;

  let walk_member_statements c member =
    let cname = match c.t with
    | ClassType s -> s
    | _ -> failwith "class does not have ClassType"
    in
    match member with
    | Field (_, _, _) -> failwith "walk_member_statements called on field"
    | Method (mname, _, _, _, _, statements) ->
      let mr = method_frame_table#get (mangle_name cname mname false) in
      curr_local_offset := min(-data_sz) (-(mr.size));
      min_local_offset := !curr_local_offset;
      offset_mgr#push mr.local_offset_table;
      List.iter (walk_statement mr) statements;
      ignore(offset_mgr#pop);
      mr.statements <- List.rev mr.statements;
      (* Because offset starts at data_sz *)
      mr.size <- -(!min_local_offset) - data_sz

    | Constructor (_, _, _, _, statements) ->
      let mr = method_frame_table#get (mangle_name cname "" true) in
      curr_local_offset := min (-data_sz) (-(mr.size));
      min_local_offset := !curr_local_offset;
      offset_mgr#push mr.local_offset_table;
      List.iter (walk_statement mr) statements;
      ignore(offset_mgr#pop);
      mr.statements <- ReturnStatement(Some (IdVal("this"))) :: mr.statements;
      mr.statements <- List.rev mr.statements;
      (* Because offset starts at data_sz *)
      mr.size <- -(!min_local_offset) - data_sz
  in

  let walk_class_statements c =
    c.methodTable#iter (fun _ v -> walk_member_statements c v);
    walk_member_statements c c.constructor
  in

  List.iter walk_class_statements classlist