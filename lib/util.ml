(* Composition: (f %> g) x === g (f x)
  (%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let ( %> ) f g x = g (f x)

(* Backwards (mathematical) composition: (f % g) x === f (g x)
 (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let ( % ) f g x = f (g x)
