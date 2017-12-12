
open Globals

class ['a] symbol_table = object

  val mutable table : (string, 'a) Hashtbl.t = Hashtbl.create 0

  (* TODO the fail-on-repeated-put behavior is janky outside the context of the typechecker *)
  method put (k : string) (v : 'a) =
    if Hashtbl.mem table k then
      type_err_list := ("Previously declared: " ^ k) :: !type_err_list
    else
      Hashtbl.add table k v

  method get (k : string) : 'a =
    try
      Hashtbl.find table k
    with
      | Not_found -> failwith (k ^ " not found")

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

  method lookup_opt name : 'a option =
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

  method lookup name : 'a =
    let sl = ref [] in
    Stack.iter (fun x -> sl := x :: !sl) stack;

    let rec loop = function
      | [] -> raise Not_found
      | h :: t ->
        (match h#get_opt (name) with
          | Some v -> v
          | None -> loop (t))
    in
    loop (!sl)

end
