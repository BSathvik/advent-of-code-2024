let file = "data/p3.txt"

open List

let part1 memory =
  let mul_r = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|} in

  let rec total start =
    try
      let start = Str.search_forward mul_r memory start in
      let mul =
        (int_of_string @@ Str.matched_group 1 memory)
        * (int_of_string @@ Str.matched_group 2 memory)
      in
      mul + total (1 + start)
    with _ -> 0
  in

  total 0

type op = Mul of int * int | Do | Dont

let part2 memory =
  let mul_r = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|} in

  let rec mult_with_pos start =
    try
      let start = Str.search_forward mul_r memory start in
      let mul =
        Mul
          ( int_of_string @@ Str.matched_group 1 memory,
            int_of_string @@ Str.matched_group 2 memory )
      in
      (start, mul) :: mult_with_pos (1 + start)
    with _ -> []
  in

  let rec switch_pos start r =
    try
      let start = Str.search_forward r memory start in
      start :: switch_pos (1 + start) r
    with _ -> []
  in

  let rec total enabled = function
    | [] -> 0
    | Do :: t -> total true t
    | Dont :: t -> total false t
    | Mul (a, b) :: t ->
        if enabled then (a * b) + total enabled t else total enabled t
  in

  mult_with_pos 0
  @ map (fun pos -> (pos, Do)) (switch_pos 0 (Str.regexp {|do()|}))
  @ map (fun pos -> (pos, Dont)) (switch_pos 0 (Str.regexp {|don't()|}))
  |> sort (fun (a, _) (b, _) -> Int.compare a b)
  |> map (fun (_, op) -> op)
  |> total true

let () =
  let memory = open_in file |> In_channel.input_all in

  Format.printf "Part 1: %d\n" (part1 memory);
  Format.printf "Part 2: %d\n" (part2 memory)
