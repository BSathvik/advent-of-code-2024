let file = "data/p4.txt"

open List

let part1 lines =
  let char (i, j) = nth (nth lines i) j in
  let row, col = (length lines, length (nth lines 0)) in

  let horizontal = Seq.init row (fun i -> Seq.init col (fun j -> (i, j))) in
  let vertical = Seq.init col (fun j -> Seq.init row (fun i -> (i, j))) in

  let down =
    Seq.append
      (Seq.init row (fun i ->
           Seq.init (min (row - i) col) (fun k -> (i + k, k))))
      (Seq.init (col - 1) (fun j ->
           Seq.init (min row (col - j - 1)) (fun k -> (k, j + k + 1))))
  in

  let up =
    Seq.append
      (Seq.init row (fun i -> Seq.init (min (i + 1) col) (fun k -> (i - k, k))))
      (Seq.init (col - 1) (fun j ->
           Seq.init (min row (col - j - 1)) (fun k -> (row - k - 1, j + k + 1))))
  in

  let rec count seq =
    match String.of_seq (Seq.map char (Seq.take 4 seq)) with
    | "" -> 0
    | "XMAS" | "SAMX" -> 1 + count (Seq.drop 1 seq)
    | _ -> count (Seq.drop 1 seq)
  in

  print_int
    (Seq.fold_left
       (fun acc seq -> acc + count seq)
       0
       ([ horizontal; vertical; down; up ] |> to_seq |> Seq.concat))

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> map (fun line -> String.to_seq line |> of_seq)
  in

  part1 lines
