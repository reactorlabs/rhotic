open Containers

module Array = struct
  include Array

  let filter_mapi f a = a |> Array.mapi f |> filter_map Fun.id

  let is_empty a = length a = 0
end

module List = struct
  include List

  let same_elts cmp l1 l2 =
    let l1', l2' = (List.sort cmp l1, List.sort cmp l2) in
    List.equal (fun x y -> cmp x y = 0) l1' l2'
end

module Option = struct
  include Option

  let bind2 f x y = Option.bind x (fun x -> Option.bind y (fun y -> f x y))
end

module Stack = struct
  include Stack

  let add_list s l = List.iter (fun x -> push x s) (List.rev l)

  let to_list s = s |> to_seq |> List.of_seq
end

module Vector = struct
  include Vector

  let foldi f acc vec =
    let g (a, i) v = (f a i v, i + 1) in
    let res, _ = Vector.fold g (acc, 0) vec in
    res
end
