let file = "p1.txt"

let () =
  let lines = 
    let file_in = open_in file in
    In_channel.input_lines file_in;
  in

  let split = List.map (fun line ->
   String.split_on_char ' ' line
    |> List.filter ( fun s -> s <> "" )
    |> List.map int_of_string
  ) lines in

  let sorted_column i =
    List.map (fun pair -> List.nth pair i) split
    |> List.sort Int.compare in 

  let sorterd_a = sorted_column 0 in
  let sorterd_b = sorted_column 1 in

  let distances = List.mapi (
    fun i id -> (List.nth sorterd_b i) - id |> abs 
  ) sorterd_a in

  print_int (List.fold_left (fun acc dis -> acc + dis) 0 distances)

