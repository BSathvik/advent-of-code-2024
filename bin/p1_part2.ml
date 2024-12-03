let file = "p1.txt"

module IntMap = Map.Make (Int)

let () =
  let lines =
    let file_in = open_in file in
    In_channel.input_lines file_in
  in

  let split =
    List.map
      (fun line ->
        String.split_on_char ' ' line
        |> List.filter (fun s -> s <> "")
        |> List.map int_of_string)
      lines
  in

  let sorted_column i =
    List.map (fun pair -> List.nth pair i) split |> List.sort Int.compare
  in

  let col_a = sorted_column 0 in
  let col_b = sorted_column 1 in

  let freq_b =
    List.fold_left
      (fun acc id ->
        IntMap.update id
          (function Some count -> Some (count + 1) | None -> Some 1)
          acc)
      IntMap.empty col_b
  in

  let total =
    List.fold_left
      (fun acc id ->
        match IntMap.find_opt id freq_b with
        | Some count -> acc + (id * count)
        | None -> acc)
      0 col_a
  in

  print_int total
