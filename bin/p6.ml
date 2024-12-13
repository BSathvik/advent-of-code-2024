let file = "data/p6.txt"

module IntSet = Set.Make (Int)

module IntTupleSet = Set.Make (struct
  type t = int * int

  let compare (ai, aj) (bi, bj) = compare (ai - aj) (bj - bi)
end)

open List

let turn = function
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | _ -> raise (Invalid_argument "This shouln't happen")

let player lines =
  let row, col = (List.length lines, List.length (List.nth lines 0)) in
  let chr (i, j) = nth (nth lines i) j in

  let pos =
    Seq.init row (fun i -> Seq.init col (fun j -> (i, j)))
    |> Seq.concat
    |> Seq.find (fun pos -> match chr pos with '#' | '.' -> false | _ -> true)
    |> Option.get
  in

  let vec =
    match chr pos with
    | '^' -> (-1, 0)
    | '>' -> (0, 1)
    | 'v' -> (1, 0)
    | '<' -> (0, -1)
    | _ -> raise (Invalid_argument "This shouln't happen")
  in

  (pos, vec)

let part1 lines =
  let row, col = (List.length lines, List.length (List.nth lines 0)) in

  let chr (i, j) = nth (nth lines i) j in
  let uniq (i, j) = (i * col) + j in

  let rec move (si, sj) (vi, vj) uniq_pos =
    let uniq_pos = IntSet.add (uniq (si, sj)) uniq_pos in
    let ni, nj = (si + vi, sj + vj) in

    if ni < row && nj < col then
      match chr (ni, nj) with
      | '#' -> IntSet.union (move (si, sj) (turn (vi, vj)) uniq_pos) uniq_pos
      | _ -> IntSet.union (move (ni, nj) (vi, vj) uniq_pos) uniq_pos
    else uniq_pos
  in

  let start_pos, start_vector = player lines in
  move start_pos start_vector IntSet.empty |> IntSet.to_list |> length

let part2 lines =
  let row, col = (List.length lines, List.length (List.nth lines 0)) in

  let chr (i, j) = nth (nth lines i) j in

  let pos_vecs = Hashtbl.create 123456 in
  let start_pos, start_vector = player lines in

  let rec has_cycle (si, sj) (vi, vj) =
    let vec_set =
      Hashtbl.find_opt pos_vecs (si, sj)
      |> Option.value ~default:IntTupleSet.empty
    in

    if IntTupleSet.mem (vi, vj) vec_set then true
    else
      let ni, nj = (si + vi, sj + vj) in
      if ni >= 0 && ni < row && nj >= 0 && nj < col then
        (* Format.printf "%d, %d\n" ni nj; *)
        match chr (ni, nj) with
        | '#' -> false
        | _ -> has_cycle (ni, nj) (vi, vj)
      else false
  in

  let rec move (si, sj) (vi, vj) =
    let ni, nj = (si + vi, sj + vj) in

    if ni < row && nj < col then (
      match chr (ni, nj) with
      | '#' -> move (si, sj) (turn (vi, vj))
      | _ ->
          let vec_set =
            Hashtbl.find_opt pos_vecs (si, sj)
            |> Option.value ~default:IntTupleSet.empty
          in
          Hashtbl.replace pos_vecs (si, sj) (IntTupleSet.add (vi, vj) vec_set);
          move (ni, nj) (vi, vj)
          + if has_cycle (si, sj) (turn (vi, vj)) then 1 else 0)
    else 0
  in

  move start_pos start_vector

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> List.map (fun line -> String.to_seq line |> of_seq)
  in

  Format.printf "Part 1: %d\n" (part1 lines);
  Format.printf "Part 2: %d\n" (part2 lines)
