let file = "data/p9.txt"

open List

let part1 disk =
  let len = length disk in

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

  let rec checksum blocks rev_files i =
    match (Seq.uncons blocks, Seq.uncons rev_files) with
    | None, _ -> 0
    | Some (None, _), None -> 0
    | Some (Some blk, tblk), _ -> (
        match Seq.uncons tblk with
        | None ->
            print_int blk;
            (i * blk) + checksum tblk rev_files (i + 1)
        | Some (_, _) -> (i * blk) + checksum tblk rev_files (i + 1))
    | Some (None, tblk), Some (f, tf) ->
        print_int f;
        (i * f) + checksum tblk tf (i + 1)
  in

  checksum blocks rev_files 0

let () =
  let disk =
    open_in file |> In_channel.input_line |> Option.get |> String.to_seq
    |> Seq.map (fun i -> String.make 1 i |> int_of_string)
    |> List.of_seq
  in

  Format.printf "\nPart 1: %d\n" (part1 disk)
