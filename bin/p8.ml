let file = "data/p8.txt"

module CharMap = Map.Make (Char)

module IntTupleMap = Map.Make (struct
  type t = int * int

  let compare a b = compare a b
end)

module IntTupleSet = Set.Make (struct
  type t = int * int

  let compare a b = compare a b
end)

open List

let part1 lines antenna_map =
  let dist (ai, aj) (bi, bj) = (bi - ai, bj - aj) in
  let row, col = (length lines, length (nth lines 0)) in

  let anodes nums =
    let len = length nums in
    Seq.init (len - 1) (fun i ->
        Seq.init (len - i - 1) (fun j -> (i, j + i + 1)))
    |> Seq.concat
    |> Seq.map (fun (i, j) ->
           let (ai, aj), (bi, bj) = (nth nums i, nth nums j) in
           let di, dj = dist (ai, aj) (bi, bj) in
           to_seq [ (ai - di, aj - dj); (bi + di, bj + dj) ])
    |> Seq.concat
    |> Seq.filter (fun (i, j) -> i >= 0 && j >= 0 && i < row && j < col)
    |> Seq.fold_left
         (fun acc (i, j) -> IntTupleSet.add (i, j) acc)
         IntTupleSet.empty
  in

  CharMap.fold
    (fun _ nums acc -> IntTupleSet.union acc (anodes nums))
    antenna_map IntTupleSet.empty
  |> IntTupleSet.to_list |> length

let part2 lines antenna_map =
  let dist (ai, aj) (bi, bj) = (bi - ai, bj - aj) in
  let row, col = (length lines, length (nth lines 0)) in

  let valid_freq_points (i, j) (di, dj) =
    let rec till_valid (i, j) (vi, vj) points =
      if i >= 0 && i < row && j >= 0 && j < col then
        let points = IntTupleSet.add (i, j) points in
        IntTupleSet.union points (till_valid (i + vi, j + vj) (vi, vj) points)
      else points
    in
    IntTupleSet.union
      (till_valid (i, j) (di, dj) IntTupleSet.empty)
      (till_valid (i, j) (-di, -dj) IntTupleSet.empty)
  in

  let anodes nums =
    let len = length nums in
    Seq.init (len - 1) (fun i ->
        Seq.init (len - i - 1) (fun j -> (i, j + i + 1)))
    |> Seq.concat
    |> Seq.map (fun (i, j) ->
           let (ai, aj), (bi, bj) = (nth nums i, nth nums j) in
           let di, dj = dist (ai, aj) (bi, bj) in
           valid_freq_points (ai, aj) (di, dj))
    |> Seq.fold_left
         (fun acc set -> IntTupleSet.union set acc)
         IntTupleSet.empty
  in

  CharMap.fold
    (fun _ nums acc -> IntTupleSet.union acc (anodes nums))
    antenna_map IntTupleSet.empty
  |> IntTupleSet.to_list |> length

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> map (fun line -> String.to_seq line |> of_seq)
  in

  let row, col = (length lines, length (nth lines 0)) in
  let chr (i, j) = nth (nth lines i) j in

  let antenna_map =
    Seq.init row (fun i -> Seq.init col (fun j -> (i, j)))
    |> Seq.concat
    |> Seq.fold_left
         (fun acc (i, j) ->
           if chr (i, j) <> '.' then
             CharMap.update
               (chr (i, j))
               (function
                 | Some ants -> Some ((i, j) :: ants) | None -> Some [ (i, j) ])
               acc
           else acc)
         CharMap.empty
  in

  Format.printf "Part 1: %d\n" (part1 lines antenna_map);
  Format.printf "Part 2: %d\n" (part2 lines antenna_map)
