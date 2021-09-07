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
