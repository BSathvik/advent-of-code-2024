let file = "data/p4.txt"

open List

let part1 lines =
  (* map (fun line -> String.concat "," (map str line)) lines |> iter print_endline *)
  let char (i, j) = nth (nth lines i) j in
  (* let range n = Seq.init n (fun i -> i) in *)
  (* let rev n = Seq.init n (fun i -> n - i) in *)
  let row, col = (length lines, length (nth lines 0)) in

  let horizontal = Seq.init row (fun i -> Seq.init col (fun j -> (i, j))) in
  let vertical = Seq.init col (fun j -> Seq.init row (fun i -> (i, j))) in

  (* let up = *)
  (*   Seq.map *)
  (*     (fun (s_i, s_j) -> *)
  (*       Seq.init (min (row - s_i) (col - s_j)) (fun k -> (s_i + k, s_j + k))) *)
  (*     (Seq.append *)
  (*        (Seq.init (row - 1) (fun i -> (i + 1, 0))) *)
  (*        (Seq.init col (fun j -> (0, j)))) *)
  (* in *)
  (* let down = *)
  (*   Seq.map *)
  (*     (fun (s_i, s_j) -> *)
  (*       Seq.init (min (row - s_i) (col - s_j)) (fun k -> (s_i + k, s_j - k))) *)
  (*     (Seq.append *)
  (*        (Seq.init (row - 1) (fun i -> (i + 1, 0))) *)
  (*        (Seq.init col (fun j -> (row - 1, j)))) *)
  (* in *)
  let down =
    Seq.append
      (Seq.init row (fun i ->
           Seq.init (min (row - i) col) (fun k -> (i + k, k))))
      (Seq.init (col - 1) (fun j ->
           Seq.init (min row (col - j + 1)) (fun k -> (k, j + k + 1))))
  in

  (* Seq.iter (Seq.iter (fun (i, j) -> Format.printf "%d:%d," i j)) up_diagonal; *)
  (* print_endline "------"; *)
  Seq.iter (Seq.iter (fun (i, j) -> Format.printf "%d:%d\n" i j)) down;

  (* print_endline "------"; *)
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
       ([ horizontal; vertical; down ] |> to_seq |> Seq.concat))

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> map (fun line -> String.to_seq line |> of_seq)
  in

  part1 lines
