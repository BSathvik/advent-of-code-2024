let file = "data/p3.txt"

open List
open Str

let mul_r = regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

let match_with_pos apply r str =
  let rec aux start =
    try
      let start_pos = search_forward r str start in
      let value = apply str in
      (start_pos, value) :: aux (1 + start_pos)
    with _ -> []
  in
  aux 0

type op = Mul of int * int | Do | Dont

let mult_with_pos str =
  let parse_group i s = int_of_string (matched_group i s) in
  match_with_pos (fun s -> Mul (parse_group 1 s, parse_group 2 s)) mul_r str

let rec total enabled = function
  | [] -> 0
  | Do :: t -> total true t
  | Dont :: t -> total false t
  | Mul (a, b) :: t ->
      if enabled then (a * b) + total enabled t else total enabled t

let part1 memory = mult_with_pos memory |> map (fun (_, op) -> op) |> total true

let part2 memory =
  mult_with_pos memory
  @ match_with_pos (fun _ -> Do) (regexp {|do()|}) memory
  @ match_with_pos (fun _ -> Dont) (regexp {|don't()|}) memory
  |> sort (fun (a, _) (b, _) -> Int.compare a b)
  |> map (fun (_, op) -> op)
  |> total true

let () =
  let memory = open_in file |> In_channel.input_all in

  Format.printf "Part 1: %d\n" (part1 memory);
  Format.printf "Part 2: %d\n" (part2 memory)
