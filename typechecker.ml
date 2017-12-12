
open Ast
open Globals
open Symboltable

let class_table : astClass symbol_table = new symbol_table
let scope_mgr : astType symbol_table_manager = new symbol_table_manager

let main_found = ref false

let object_class = {t = ClassType("Object"); super = None;
    constructor = make_default_ctor "Object"; fieldTable = new symbol_table;
    methodTable = new symbol_table}

let curr_class : astClass ref = ref object_class

let lookup_class name = class_table#get_opt name

let lookup_field start_class name =
  let rec loop c =
    match c.fieldTable#get_opt name with
    | None ->
      (match c.super with
      | None -> None
      | Some super -> loop super)
    | Some id -> Some id
  in
  loop start_class

let lookup_method start_class name =
  let rec loop c =
    match c.methodTable#get_opt name with
    | None ->
      (match c.super with
      | None -> None
      | Some super -> loop super)
    | Some id -> Some id
  in
  loop start_class

let rec types_equal a b =
  match a with
  | BoolType -> a == b
  | IntType -> a == b
  | CharType -> a == b
  | ClassType s1 ->
    (match b with
    | ClassType s2 -> (String.compare s1 s2) == 0
    | _ -> false)
  | ArrayType (t1, d1) ->
    (match b with
    | ArrayType (t2, d2) -> (d1 == d2) && (types_equal t1 t2)
    | _ -> false)
  | MethodType (o1, args1, r1) ->
    (match b with
    | MethodType (o2, args2, r2) ->
      let f a t1 t2 = if a then types_equal t1 t2 else false in
      (try
        types_equal o1 o2 && types_equal r1 r2 &&
          List.fold_left2 f true args1 args2
      with
        | Invalid_argument _ -> false)
    | _ -> false)
  | VoidType -> a == b

let get_parent_class_name sub_name =
  match lookup_class sub_name with
    | None -> failwith "get_parent_class called on nonexistent class"
    | Some subclass ->
      (match subclass.super with
      | None -> None
      | Some superclass ->
          (match superclass.t with
          | ClassType super_name -> Some super_name
          | _ -> failwith "class does not have ClassType"))

let rec is_subtype sub super =
  match sub with
  | BoolType -> super == BoolType
  | IntType -> (super == IntType) || (super == CharType)
  | CharType -> (super == IntType) || (super == CharType)
  | ArrayType (t1, d1) ->
    (match super with
    | ArrayType (t2, d2) -> (is_subtype t1 t2) && d1 == d2
    | _ -> false)
  | ClassType strsub ->
    (match super with
    | ClassType strsuper ->
      let rec loop name =
        if (String.compare strsuper name) == 0 then true else
        match get_parent_class_name name with
        | None -> false
        | Some newsub -> loop newsub
      in
      loop strsub
    | _ -> false)
  | VoidType -> super == VoidType
  | MethodType (o1, args1, r1) ->
    (match super with
    | MethodType (o2, args2, r2) ->
      let f a t1 t2 = if a then is_subtype t1 t2 else false in
      (try
        is_subtype o1 o2 && is_subtype r1 r2 &&
          List.fold_left2 f true args1 args2
      with
        | Invalid_argument _ -> false)
    | _ -> false)

let rec is_lvalue = function
  | PrimaryExpr p ->
    (match p with
    | NewArrayPrimary (_, _) -> false
    | IdPrimary (_, _) -> true
    | NonNewArrayPrimary (_, n) ->
      (match n with
      | ParenExpr e -> is_lvalue e
      | ArrayExpr (_, _) -> true
      | FieldExpr (_, _) -> true
      | SuperFieldExpr _ -> true
      | _ -> false))
  | _ -> false

let type_of_method c m =
  match m with
  | Field (_, _, _) -> failwith "type_of_method called on Field"
  | Method (_, ret, _, _, formals, _) ->
    let formal_types = List.map (fun (f : astFormal) -> f.t) formals in
    MethodType(c.t, formal_types, ret)
  | Constructor (ret, _, _, formals, _) ->
    let formal_types = List.map (fun (f : astFormal) -> f.t) formals in
    MethodType(c.t, formal_types, ret)

let is_main_method m =
  match m with
  | Field (_, _, _) -> failwith "is_main_method called on field"
  | Constructor (_, _, _, _, _) ->
    failwith "is_main_method called on constructor"
  | Method (name, _, _, modlist, _, _) ->
    (String.compare name "main" == 0) && (List.mem Static modlist) &&
      (types_equal (type_of_method !curr_class m)
        (MethodType(!curr_class.t, [ArrayType(ClassType("String"), 1)],
          VoidType)))

let check_constructor_statements c =
  match c.constructor with
  | Field (_, _, _) -> failwith "check constructor_statements called on field"
  | Method (_, _, _, _, _, _) ->
    failwith "check_constructor_statements called on method"
  | Constructor (t, tbl, mods, formals, statements) ->
    (* TODO clean up logic? *)
    if List.length statements > 0 then begin
      (match List.hd statements with
      | SuperStatement _ -> ()
      | _ -> c.constructor <-
        Constructor(t, tbl, mods, formals, SuperStatement([]) :: statements));
      let rec check_list l =
        (match l with
        | [] -> ()
        | hd :: tl ->
          (match hd with
          | SuperStatement _ ->
            type_err_list :=
              ("Super statement can only be at beginning of constructor: "
              ^ strMember c.constructor) :: !type_err_list
          | _ -> ());
          check_list tl)
      in
      check_list (List.tl statements) end
    else
      c.constructor <- Constructor(t, tbl, mods, formals, [SuperStatement([])])

let walk_literal = function
  | NullLiteral -> ClassType("Object")
  | BoolLiteral _ -> BoolType
  | IntLiteral _ -> IntType
  | CharLiteral _ -> CharType
  | StringLiteral _ -> ClassType("String")

let rec walk_expr = function
  | UnOpExpr (op, e) ->
    let t = walk_expr e in
    (match op with
    | UnPlus ->
      if not (is_subtype t IntType) then
        type_err_list := ("Type error: " ^ strExpr e) :: !type_err_list;
      IntType
    | UnMinus ->
      if not (is_subtype t IntType) then
        type_err_list := ("Type error: " ^ strExpr e) :: !type_err_list;
      IntType
    | Not ->
      if not (types_equal t BoolType) then
        type_err_list := ("Type error: " ^ strExpr e) :: !type_err_list;
      BoolType)

  | BinOpExpr (op, e1, e2) ->
    let t1 = walk_expr e1 in
    let t2 = walk_expr e2 in
    (match op with
    | Asgn ->
      if not (is_lvalue e1) then
        type_err_list := ("l-value required: " ^ strExpr e1) :: !type_err_list;
      if not (is_subtype t2 t1) then
        type_err_list := ("Type error: " ^ strExpr e2) :: !type_err_list;
      t1
    | Greater | Less | Geq | Leq ->
      if not (is_subtype t1 IntType) then
        type_err_list := ("Type error: " ^ strExpr e1) :: !type_err_list;
      if not (is_subtype t2 IntType) then
        type_err_list := ("Type error: " ^ strExpr e2) :: !type_err_list;
      BoolType
    | Equals | Neq ->
      if not (is_subtype t1 t2 || is_subtype t2 t1) then
        type_err_list := ("Type error: " ^ strExpr e1) :: !type_err_list;
      BoolType
    | BinPlus | BinMinus | Times | Div | Mod ->
      if not (is_subtype t1 IntType) then
        type_err_list := ("Type error: " ^ strExpr e1) :: !type_err_list;
      if not (is_subtype t2 IntType) then
        type_err_list := ("Type error: " ^ strExpr e2) :: !type_err_list;
      IntType
    | And | Or ->
      if not (types_equal t1 BoolType) then
        type_err_list := ("Type error: " ^ strExpr e1) :: !type_err_list;
      if not (types_equal t2 BoolType) then
        type_err_list := ("Type error: " ^ strExpr e2) :: !type_err_list;
      BoolType)

  | PrimaryExpr p -> walk_primary p false

and walk_nonnew nn =
  match nn with
  | LiteralExpr l -> walk_literal l
  | ThisExpr -> (!curr_class).t
  | ParenExpr e -> walk_expr e
  | NewObjExpr (s, arglist) ->
    (match lookup_class s with
    | Some c ->
      let arg_types = List.map (fun e -> walk_expr e) arglist in
      let arg_meth_type = MethodType(ClassType(s), arg_types, ClassType(s)) in
      if not (is_subtype arg_meth_type (type_of_method c c.constructor)) then
        type_err_list := ("Mismatched args: " ^ strNonNewArrayExpr nn) ::
          !type_err_list;
      c.t
    | None ->
      type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
      ClassType(s))
  | ThisCallExpr (s, arglist) ->
    (match lookup_method !curr_class s with
    | None ->
      type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
      ClassType("Object")
    | Some m ->
      (match m with
      | Field (_, _, _) -> failwith "field in method table"
      | Constructor (_, _, _, _, _) -> failwith "constructor in method table"
      | Method (_, ret, _, _, _, _) ->
        let arg_types = List.map (fun e -> walk_expr e) arglist in
        let arg_meth_type = MethodType(!curr_class.t, arg_types, ret) in
        if not (is_subtype arg_meth_type (type_of_method !curr_class m)) then
          type_err_list := ("Mismatched args: " ^ strNonNewArrayExpr nn) ::
            !type_err_list;
        ret))

  | MethodCallExpr (p, s, arglist) ->
    (match walk_primary p true with
    | ClassType n ->
      (match class_table#get_opt n with
      | None ->
        type_err_list := ("Undefined name: " ^ n) :: !type_err_list;
        ClassType("Object")
      | Some c ->
        (match lookup_method c s with
        | None ->
          type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
          ClassType("Object")
        | Some m ->
          (match m with
          | Field (_, _, _) -> failwith "field in method table"
          | Constructor (_, _, _, _, _) -> failwith "constructor in method table"
          | Method (_, ret, _, _, _, _) ->
            let arg_types = List.map (fun e -> walk_expr e) arglist in
            let arg_meth_type = MethodType(c.t, arg_types, ret) in
            if not (is_subtype arg_meth_type (type_of_method c m)) then
              type_err_list := ("Mismatched args: " ^ strNonNewArrayExpr nn) ::
                !type_err_list;
            ret)))
    | _ ->
      type_err_list := ("Type error: not an object: " ^ strPrimary p) ::
        !type_err_list;
      ClassType("Object"))
  (* | _ -> ClassType("Object") *)

  | SuperCallExpr (s, arglist) ->
    (match !curr_class.super with
    | Some super ->
      (match lookup_method super s with
      | None ->
        type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
        ClassType("Object")
      | Some m ->
        (match m with
        | Field (_, _, _) -> failwith "field in method table"
        | Constructor (_, _, _, _, _) -> failwith "constructor in method table"
        | Method (_, ret, _, modlist, _, _) ->
          let arg_types = List.map (fun e -> walk_expr e) arglist in
          let arg_meth_type = MethodType(!curr_class.t, arg_types, ret) in
          if not (types_equal (type_of_method !curr_class m) arg_meth_type) then
            type_err_list := ("Mismatched args: " ^ strNonNewArrayExpr nn) ::
              !type_err_list;
          if List.mem Private modlist then
            type_err_list :=
              ("Private super method cannot be called from subclass: " ^
              strNonNewArrayExpr nn) :: !type_err_list;
          ret))
    | None ->
      type_err_list := ("Super called on Object class") :: !type_err_list;
      ClassType("Object"))

  | ArrayExpr (p, e) ->
    (match walk_primary p false with
    | ArrayType (t, d) ->
      if  not (is_subtype (walk_expr e) IntType) then
        type_err_list := ("Type error: not an integer:" ^ strExpr e) ::
          !type_err_list;
      if d == 1 then t else ArrayType(t, d - 1)
    | _ ->
      type_err_list := ("Type error: not an array:" ^ strPrimary p) ::
        !type_err_list;
      ClassType("Object"))

  | FieldExpr (p, f) ->
    (match walk_primary p false with
    | ClassType s ->
      (match lookup_class s with
      | Some c ->
        (match lookup_field c f with
        | Some m ->
          (match m with
          | Field (_, t, _) -> t
          | _ -> failwith "method or constructor in field table")
        | None ->
          type_err_list := ("Undefined name: " ^ f) :: !type_err_list;
          ClassType("Object"))
      | None ->
        type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
        ClassType("Object"))
    | ArrayType (t, d) ->
      if String.compare f "length" == 0 then IntType else begin
        type_err_list := ("Type error: not an object: " ^ strPrimary p) ::
          !type_err_list;
        ClassType("Object") end
    | _ ->
      type_err_list := ("Type error: not an object: " ^ strPrimary p) ::
        !type_err_list;
      ClassType("Object"))

  | SuperFieldExpr f ->
    (match !curr_class.super with
    | Some super ->
      (match lookup_field super f with
      | Some m ->
        (match m with
        | Field (_, t, _) -> t
        | _ -> failwith "method or constructor in field table")
      | None ->
        type_err_list := ("Undefined name: " ^ f) :: !type_err_list;
        ClassType("Object"))
    | None ->
      type_err_list := ("Super called on Object class") :: !type_err_list;
      ClassType("Object"))

and walk_newarr (n : astNewArrayExpr) =
  let check_dim d =
    let d_t = walk_expr d in
    if not (is_subtype d_t IntType) then
      type_err_list := ("Invalid array dimension: " ^ strExpr d) ::
        !type_err_list;
  in
  (match n.t with
  | ClassType _ ->
    type_err_list := ("Initialized array type must be primitive: " ^
      strNewArrayExpr n) :: !type_err_list
  | _ -> ());
  List.iter check_dim n.dimList;
  ArrayType(n.t, List.length n.dimList)

and walk_primary p from_method_call =
  match p with
  | NewArrayPrimary (tb, n) ->
    let t = walk_newarr n in
    tb.t <- t;
    t
  | NonNewArrayPrimary (tb, n) ->
    let t = walk_nonnew n in
    tb.t <- t;
    t
  | IdPrimary (tb, s) ->
   (match scope_mgr#lookup_opt s with
    | Some t ->
      tb.t <- t;
      t
    | None ->
      (match lookup_field !curr_class s with
      | Some f ->
        (match f with
        | Field (_, t, _) ->
          tb.t <- t;
          t
        | _ -> failwith "lookup_field returned non-field")
      | None ->
        if from_method_call then
          (match lookup_class s with
          | Some c ->
            tb.t <- c.t;
            c.t
          | None -> type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
            (* TODO what to return here? *)
            ClassType("Object"))
        else begin
        type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
        (* TODO what to return here? *)
        ClassType("Object")
        end))

let walk_vardecl v =
  match v.expr with
  | Some e ->
    let t = walk_expr e in
    if v.dim == 0 then
      Some t
    else
      (match t with
      | ArrayType (at, dim) ->
        if dim != v.dim then
          type_err_list := ("Array has wrong dimension: " ^ strExpr e) ::
            !type_err_list;
        Some (ArrayType(at, dim))
      | _ ->
        type_err_list := ("Declarator not an array: " ^ strExpr e) ::
          !type_err_list;
        Some (ArrayType(t, v.dim)))
  | None -> None

let check_type_exists t =
  let check_valid_class s =
    match class_table#get_opt s with
    | Some _ -> ()
    | None ->
      type_err_list := ("Undefined name: " ^ s) :: !type_err_list;
  in
  match t with
  | ClassType s -> check_valid_class s
  | ArrayType (t, _) ->
    (match t with
    | ClassType s -> check_valid_class s
    | ArrayType (_, _) -> failwith "ArrayType contains ArrayType"
    | MethodType (_, _, _) -> failwith "ArrayType contains MethodType"
    | _ ->())
  | MethodType (_, _, _) -> failwith "MethodType found in type declaration"
  | _ -> ()

(* TODO fix to support c-style array declaration syntax *)
let check_vardecl t v =
  match walk_vardecl v with
  | Some vt ->
    if not (is_subtype vt t) then
      type_err_list := ("Type error: " ^ strVarDecl v) :: !type_err_list;
  | None -> ()

let check_vardecl_field v =
  match walk_vardecl v with
  | Some _ ->
    type_err_list := ("Field initializers illegal: " ^ strVarDecl v) ::
      !type_err_list
  | None -> ()

let walk_formal (f : astFormal) =
  check_type_exists f.t

let rec walk_statement st =
  match st with
  | EmptyStatement | ContinueStatement | BreakStatement -> ()

  | DeclStatement (t, vardecls) ->
    check_type_exists t;
    List.iter (check_vardecl t) vardecls;
    List.iter (fun d -> scope_mgr#top#put d.name t) vardecls

  | IfStatement (iftbl, elsetbl_o, cond, ifs, elses_o) ->
    let t = walk_expr cond in
    if not (types_equal t BoolType) then
      type_err_list := ("Invalid if condition: " ^ strExpr cond) ::
        !type_err_list;
    scope_mgr#push iftbl;
    walk_statement ifs;
    ignore(scope_mgr#pop);
    (match elses_o with
    | Some elses ->
      (match elsetbl_o with
      | None -> failwith "If statement has else branch but no else symbol table"
      | Some elsetbl ->
        scope_mgr#push elsetbl;
        walk_statement elses;
        ignore(scope_mgr#pop))
    | None -> ())

  | ExprStatement e -> ignore(walk_expr e)

  | WhileStatement (tbl, cond, s) ->
    let t = walk_expr cond in
    if not (types_equal t BoolType) then
      type_err_list := ("Invalid while condition: " ^ strExpr cond) ::
        !type_err_list;
    scope_mgr#push tbl;
    walk_statement s;
    ignore(scope_mgr#pop)

  | ReturnStatement e_o ->
    (match e_o with
    | Some e -> ignore(walk_expr e)
    | None -> ());

  | BlockStatement (tbl, statements) ->
    scope_mgr#push tbl;
    List.iter walk_statement statements;
    ignore(scope_mgr#pop)

  | SuperStatement (exprs) ->
    (match !curr_class.super with
    | None -> failwith "object class has super statement in constructor"
    | Some s ->
      let desired_t = type_of_method s s.constructor in
      let param_ts = List.map walk_expr exprs in
      let actual_t = MethodType(s.t, param_ts, s.t) in
      if not (is_subtype actual_t desired_t) then
        type_err_list := ("Super args do not match superclass constructor: " ^
          "superclass: " ^ strType s.t ^
          "; subclass: " ^ strType !curr_class.t) :: !type_err_list)

let walk_member m =
  match m with
  | Field (modlist, t, vardecl) ->
    if List.mem Static modlist then begin
      if List.length modlist > 2 then
        type_err_list := ("Multiple access modifiers illegal: " ^ strMember m)
          :: !type_err_list;
      type_err_list := ("Field cannot be static: " ^ strMember m) ::
        !type_err_list
    end
    else if List.length modlist > 1 then
        type_err_list := ("Multiple access modifiers illegal: " ^ strMember m)
          :: !type_err_list;
    check_type_exists t;
    check_vardecl_field vardecl

  | Method (name, t, tbl, modlist, formallist, statementlist) ->
    if not (!main_found) then begin
      if is_main_method m then main_found := true end
    else if is_main_method m then
      type_err_list := "Program has multiple main methods" :: !type_err_list;

    if List.mem Static modlist then begin 
      if List.length modlist > 2 then
        type_err_list := ("Multiple access modifiers illegal: " ^ strMember m)
          :: !type_err_list
      end
    else if List.length modlist > 1 then
        type_err_list := ("Multiple access modifiers illegal: " ^ strMember m)
          :: !type_err_list;
    check_type_exists t;
    List.iter walk_formal formallist;

    (match !curr_class.super with
    | None -> ()
    | Some super ->
      (match lookup_method super name with
      | None -> ()
      | Some super_method->

        (match super_method with
        | Method (_, _, _, s_modlist, _, _) ->
          if not (List.mem Static s_modlist ||
            types_equal
              (type_of_method !curr_class m)
              (type_of_method !curr_class super_method)) then
            type_err_list := ("Overridden method type does not match super: " ^
              strMember m) :: !type_err_list;
        | _ -> failwith "method returned from lookup_method not a method")));
    scope_mgr#push tbl;
    List.iter walk_statement statementlist;
    ignore(scope_mgr#pop)

  | Constructor (t, tbl, modlist, formallist, statementlist) ->
    check_type_exists t;
    List.iter walk_formal formallist;
    scope_mgr#push tbl;
    List.iter walk_statement statementlist;
    ignore(scope_mgr#pop)

let walk_class n c =

  let discard_fst _ b = walk_member b in

  curr_class := c;
  (* TODO jank jank jank *)
  if (String.compare n "Object") != 0 then
    check_constructor_statements c;
  c.fieldTable#iter discard_fst;
  c.methodTable#iter discard_fst;
  walk_member c.constructor


let type_check tree = 
  tree#iter (fun n c -> walk_class n c);
  if not (!main_found) then
    type_err_list := "No main method in program" :: !type_err_list;
  !type_err_list
