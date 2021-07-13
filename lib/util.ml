(* Composition: (f %> g) x === g (f x)
  (%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let ( %> ) f g x = g (f x)

(* Backwards (mathematical) composition: (f % g) x === f (g x)
 (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let ( % ) f g x = f (g x)

module Array = struct
  include Array

  let filter f a = a |> Array.to_list |> List.filter f |> Array.of_list

  let filter_map f a = a |> Array.to_list |> List.filter_map f |> Array.of_list

  let filter_mapi f a = a |> Array.mapi f |> filter_map Fun.id
end

module Map = struct
  module Make (O : Map.OrderedType) = struct
    module M = Map.Make (O)

    include M

    let get_or k m ~default =
      match find_opt k m with
      | None -> default
      | Some v -> v
  end
end

module Option = struct
  include Option

  let bind2 f x y = Option.bind x (fun x -> Option.bind y (fun y -> f x y))

  let get_or ~default = function
    | None -> default
    | Some x -> x

  let map_or ~default f = function
    | None -> default
    | Some x -> f x
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
