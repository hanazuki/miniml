type 'a t

exception Not_bound
exception Not_ready

val empty : 'a t
val extend : Syntax.id -> 'a -> 'a t -> 'a t
val lookup : Syntax.id -> 'a t -> 'a
val reserve : Syntax.id -> 'a t -> 'a t
val emplace : Syntax.id -> 'a -> 'a t -> unit
val fold : (Syntax.id -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
