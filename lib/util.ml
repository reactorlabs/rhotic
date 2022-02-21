open Containers

module Array = struct
  include Array

  let filter_mapi f a = a |> Array.mapi f |> filter_map Fun.id

  let is_empty a = length a = 0
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
