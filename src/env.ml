open Syntax

type 'a t = 'a option ref IdMap.t

exception Not_bound
exception Not_ready

(** empty environment *)
let empty = IdMap.empty

(** adds an entry for [id] with value [x] *)
let extend id x = IdMap.add id (ref (Some x))

(** finds an entry for [id] *)
let lookup id env =
  let r = try IdMap.find id env with
    | Not_found -> raise Not_bound in
  match !r with
    | None -> raise Not_ready
    | Some x -> x

(** creates a thunk for [id] for back-patching *)
let reserve id = IdMap.add id (ref None)

(** sets a value into a thunk created with [reserve] *)
let emplace id x env =
  let r = try IdMap.find id env with
    | Not_found -> assert false in
  match !r with
    | None -> r := Some x
    | Some _ -> assert false

let fold f = IdMap.fold (fun k r a -> match !r with Some x -> f k x a | _ -> a)
