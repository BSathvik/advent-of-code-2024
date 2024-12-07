let file = "data/p4.txt"

open Seq

let part1 lines =
  let row, col = (List.length lines, List.length (List.nth lines 0)) in

  let rec count seq =
    match
      take 4 seq
      |> map (fun (i, j) -> List.nth (List.nth lines i) j)
      |> String.of_seq
    with
    | "" -> 0
    | "XMAS" | "SAMX" -> 1 + (drop 1 seq |> count)
    | _ -> drop 1 seq |> count
  in

  [
    init row (fun i -> init col (fun j -> (i, j)));
    init col (fun j -> init row (fun i -> (i, j)));
    init row (fun i -> init (min (row - i) col) (fun k -> (i + k, k)));
    init (col - 1) (fun j ->
        init (min row (col - j - 1)) (fun k -> (k, j + k + 1)));
    init row (fun i -> init (min (i + 1) col) (fun k -> (i - k, k)));
    init (col - 1) (fun j ->
        init (min row (col - j - 1)) (fun k -> (row - k - 1, j + k + 1)));
  ]
  |> List.to_seq |> concat |> map count |> fold_left ( + ) 0

let part2 lines =
  let row, col = (List.length lines, List.length (List.nth lines 0)) in

  map
    (fun (i, j) ->
      match
        [ (0, 0); (1, 1); (2, 2); (0, 2); (1, 1); (2, 0) ]
        |> List.map (fun (r, c) -> (i + r, j + c))
        |> List.map (fun (i, j) -> List.nth (List.nth lines i) j)
        |> List.to_seq |> String.of_seq
      with
      | "MASMAS" | "SAMSAM" | "SAMMAS" | "MASSAM" -> 1
      | _ -> 0)
    (init (row - 2) (fun i -> init (col - 2) (fun j -> (i, j))) |> concat)
  |> fold_left ( + ) 0

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> List.map (fun line -> String.to_seq line |> List.of_seq)
  in

  print_int (part1 lines);
  print_endline "----";
  print_int (part2 lines)
