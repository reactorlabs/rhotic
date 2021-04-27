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

module String = struct
  include String

  let prefix ~pre s =
    let len_p = String.length pre in
    if len_p > String.length s then false
    else
      let rec aux i =
        if i = len_p then true
        else if String.unsafe_get pre i <> String.unsafe_get s i then false
        else aux (i + 1) in
      aux 0

  let chop_prefix ~pre s =
    if prefix ~pre s then
      let len_p = String.length pre in
      Some (String.sub s len_p (String.length s - len_p))
    else None
end
