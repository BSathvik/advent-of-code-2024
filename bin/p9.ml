let file = "data/p9.txt"

open List

let part1 disk =
  let len = length disk in

  let file_blocks_len =
    disk |> filteri (fun i _ -> i mod 2 = 0) |> fold_left ( + ) 0
  in

  let rev_files =
    Seq.unfold
      (fun (i, id) ->
        if i >= 0 then
          Some (Seq.init (nth disk i) (fun _ -> id), (i - 2, id - 1))
        else None)
      (len - 1 - ((len - 1) mod 2), (len - (len mod 2)) / 2)
    |> Seq.concat
  in

  let blocks =
    Seq.unfold
      (fun (i, id) ->
        if i < len then
          if i mod 2 = 0 then
            Some (Seq.init (nth disk i) (fun _ -> Some id), (i + 1, id + 1))
          else Some (Seq.init (nth disk i) (fun _ -> None), (i + 1, id))
        else None)
      (0, 0)
    |> Seq.concat
  in

  let rec checksum blocks rev_files i emtpy_n files_n =
    if emtpy_n + files_n >= file_blocks_len then 0
    else
      match (Seq.uncons blocks, Seq.uncons rev_files) with
      | None, _ -> 0
      | Some (None, _), None -> 0
      | Some (Some blk, tblk), _ -> (
          match Seq.uncons tblk with
          | None -> i * blk
          | Some (_, _) ->
              (i * blk) + checksum tblk rev_files (i + 1) emtpy_n (files_n + 1))
      | Some (None, tblk), Some (f, tf) ->
          (i * f) + checksum tblk tf (i + 1) (emtpy_n + 1) files_n
  in

  checksum blocks rev_files 0 0 0

let () =
  let disk =
    open_in file |> In_channel.input_line |> Option.get |> String.to_seq
    |> Seq.map (fun i -> String.make 1 i |> int_of_string)
    |> List.of_seq
  in

  Format.printf "Part 1: %d\n" (part1 disk)
