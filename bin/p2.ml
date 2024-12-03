let file = "data/p2.txt"

open List

let is_safe report =
  let rec diffs = function
    | [] | _ :: [] -> []
    | x :: y :: t -> (y - x) :: diffs (y :: t)
  in

  for_all (fun n -> n > 0 && n <= 3) (diffs report)
  || for_all (fun n -> n < 0 && n >= -3) (diffs report)

let part1 reports = filter is_safe reports |> length

let part2 reports =
  let remove i l = filteri (fun j _ -> i <> j) l in
  let report_combos r = init (length r) (fun i -> remove i r) in

  filter (fun report -> exists is_safe (report_combos report)) reports |> length

let () =
  let reports =
    map
      (fun line -> String.split_on_char ' ' line |> map int_of_string)
      (open_in file |> In_channel.input_lines)
  in

  Format.printf "Part 1: %d\n" (part1 reports);
  Format.printf "Part 2: %d" (part2 reports)
