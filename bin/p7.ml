let file = "data/p7.txt"

open List

let sum_mul equations =
  let rec check_valid test = function
    | [] -> false
    | [ n1 ] -> n1 = test
    | n1 :: n2 :: t ->
        check_valid test ((n1 + n2) :: t) || check_valid test ((n1 * n2) :: t)
  in

  equations
  |> filter (fun (test, nums) -> check_valid test nums)
  |> map (fun (test, _) -> test)
  |> fold_left ( + ) 0

let sum_mul_concat equations =
  let rec check_valid test = function
    | [] -> false
    | [ n1 ] -> n1 = test
    | n1 :: n2 :: t ->
        let concat_num =
          int_of_string @@ String.cat (string_of_int n1) (string_of_int n2)
        in
        check_valid test ((n1 + n2) :: t)
        || check_valid test ((n1 * n2) :: t)
        || check_valid test (concat_num :: t)
  in

  equations
  |> filter (fun (test, nums) -> check_valid test nums)
  |> map (fun (test, _) -> test)
  |> fold_left ( + ) 0

let () =
  let lines =
    open_in file |> In_channel.input_lines
    |> map (fun line ->
           match String.split_on_char ':' line with
           | test :: nums :: _ ->
               ( int_of_string test,
                 String.trim nums |> String.split_on_char ' '
                 |> map int_of_string )
           | _ -> raise (Invalid_argument "this shouldn't happen"))
  in

  Format.printf "Part 1: %d\n" (sum_mul lines);
  Format.printf "Part 2: %d\n" (sum_mul_concat lines)
