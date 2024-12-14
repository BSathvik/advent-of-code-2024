let file = "data/p6.txt"

module IntSet = Set.Make (Int)

module IntTupleSet = Set.Make (struct
  type t = int * int

  let compare (ai, aj) (bi, bj) = compare (ai, aj) (bi, bj)
end)

module IntTupleMap = Map.Make (struct
  type t = int * int

  let compare (ai, aj) (bi, bj) = compare (ai, aj) (bi, bj)
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
  let start_pos, start_vector = player lines in

  let rec has_cycle (si, sj) (oi, oj) (vi, vj) pos_vecs =
    let vec_set =
      IntTupleMap.find_opt (si, sj) pos_vecs
      |> Option.value ~default:IntTupleSet.empty
    in
    let pos_vecs =
      IntTupleMap.add (si, sj) (IntTupleSet.add (vi, vj) vec_set) pos_vecs
    in

    if IntTupleSet.mem (vi, vj) vec_set then true
    else
      let ni, nj = (si + vi, sj + vj) in
      if ni >= 0 && ni < row && nj >= 0 && nj < col then
        match chr (ni, nj) with
        | '#' -> has_cycle (si, sj) (oi, oj) (turn (vi, vj)) pos_vecs
        | _ ->
            if (ni, nj) = (oi, oj) then
              has_cycle (si, sj) (oi, oj) (turn (vi, vj)) pos_vecs
            else
              (* Format.printf "%d, %d\n" si sj; *)
              has_cycle (ni, nj) (oi, oj) (vi, vj) pos_vecs
      else false
  in

  let rec move (si, sj) (vi, vj) pos_vecs res =
    let vec_set =
      IntTupleMap.find_opt (si, sj) pos_vecs
      |> Option.value ~default:IntTupleSet.empty
    in
    let pos_vecs =
      IntTupleMap.add (si, sj) (IntTupleSet.add (vi, vj) vec_set) pos_vecs
    in

    let ni, nj = (si + vi, sj + vj) in
    if
      ni >= 0 && nj >= 0 && ni < row && nj < col
      && not (IntTupleSet.mem (vi, vj) vec_set)
    then
      match chr (ni, nj) with
      | '#' -> move (si, sj) (turn (vi, vj)) pos_vecs res
      | _ ->
          let res =
            if
              (not (IntTupleMap.mem (ni, nj) pos_vecs))
              && has_cycle (si, sj) (ni, nj) (turn (vi, vj)) pos_vecs
            then IntTupleSet.add (ni, nj) res
            else res
          in
          move (ni, nj) (vi, vj) pos_vecs res
    else res
  in

  move start_pos start_vector IntTupleMap.empty IntTupleSet.empty
  |> IntTupleSet.to_list |> length

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> List.map (fun line -> String.to_seq line |> of_seq)
  in

  Format.printf "Part 1: %d\n" (part1 lines);
  Format.printf "Part 2: %d\n" (part2 lines)
