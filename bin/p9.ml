let file = "data/p9.txt"

open List

let part1 disk =
  let len = length disk in

  let files, free =
    Seq.init len (fun i -> (i, nth disk i))
    |> Seq.partition_map (fun (i, n) ->
           if i mod 2 = 0 then Either.Left (i, n) else Either.Right (i, n))
  in

  let rec checksum files free = 

  0

let () =
  let disk =
    open_in file |> In_channel.input_line |> Option.get |> String.to_seq
    |> of_seq |> map int_of_char
  in

  Format.printf "Part 1: %d\n" (part1 disk)
