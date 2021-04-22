(* Composition: (f %> g) x === g (f x)
  (%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let ( %> ) f g x = g (f x)

(* Backwards (mathematical) composition: (f % g) x === f (g x)
 (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let ( % ) f g x = f (g x)

module Option = struct
  include Option

  let bind2 f x y = Option.bind x (fun x -> Option.bind y (fun y -> f x y))
end
