let file = "data/p6.txt"

module IntSet = Set.Make (Int)
open List

let part1 lines =
  let row, col = (List.length lines, List.length (List.nth lines 0)) in

  let chr (i, j) = nth (nth lines i) j in
  let uniq (i, j) = (i * col) + j in
  let turn = function
    | -1, 0 -> (0, 1)
    | 0, 1 -> (1, 0)
    | 1, 0 -> (0, -1)
    | 0, -1 -> (-1, 0)
    | _ -> raise (Invalid_argument "This shouln't happen")
  in

  let rec move (si, sj) (vi, vj) uniq_pos =
    let uniq_pos = IntSet.add (uniq (si, sj)) uniq_pos in
    let ni, nj = (si + vi, sj + vj) in

    if ni < row && nj < col then
      match chr (ni, nj) with
      | '#' -> IntSet.union (move (si, sj) (turn (vi, vj)) uniq_pos) uniq_pos
      | _ -> IntSet.union (move (ni, nj) (vi, vj) uniq_pos) uniq_pos
    else uniq_pos
  in

  let start_pos =
    Seq.init row (fun i -> Seq.init col (fun j -> (i, j)))
    |> Seq.concat
    |> Seq.find (fun pos -> match chr pos with '#' | '.' -> false | _ -> true)
    |> Option.get
  in

  let start_vector =
    match chr start_pos with
    | '^' -> (-1, 0)
    | '>' -> (0, 1)
    | 'v' -> (1, 0)
    | '<' -> (0, -1)
    | _ -> raise (Invalid_argument "This shouln't happen")
  in

  move start_pos start_vector IntSet.empty |> IntSet.to_list |> length

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> List.map (fun line -> String.to_seq line |> of_seq)
  in

  Format.printf "Part 1: %d" (part1 lines)
