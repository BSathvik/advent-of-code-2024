let file = "data/p6.txt"

module IntSet = Set.Make (Int)
open List

let part1 lines =
  let row, col = (List.length lines, List.length (List.nth lines 0)) in

  (* let uniq_pos = IntSet.empty in *)
  let chr (i, j) = nth (nth lines i) j in

  let rec move start vec uniq_pos =
    match chr start with |
      "." -> 
  in


let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> List.map (fun line -> String.to_seq line)
  in

  print_int (part1 lines);
  print_endline "----"
