let file = "data/p5.txt"

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
open List

(* type order = (int, order) hashtbl.t | leaf of int *)

type graph = Node of (int * graph t) | Leaf of int

let order_scores orders =
  let node_table = Hashtbl.create 123456 in
  let root_nums =
    ref (map (fun (before, _) -> before) orders |> IntSet.of_list)
  in

  List.iter
    (fun (before, after) ->
      let before_node =
        match Hashtbl.find_opt node_table before with
        | Some node -> node
        | None ->
            let node = ref (Node (before, [])) in
            Hashtbl.add node_table before node;
            node
      in
      let after_node =
        match Hashtbl.find_opt node_table after with
        | Some node -> node
        | None ->
            let node = ref (Leaf after) in
            Hashtbl.add node_table after node;
            node
      in

      (* `after` num cannot be a candidate for the root node *)
      root_nums := IntSet.remove after !root_nums;

      print_endline "")
    orders;

  print_endline "done"

let part1 orders updates =
  iter (fun (i, j) -> Format.printf "%d|%d\n" i j) orders;
  iter
    (fun l -> String.concat "," (map string_of_int l) |> print_endline)
    updates;

  order_scores orders;

  (* let rec increases = function *)
  (*   | [] | [ _ ] -> true *)
  (*   | a :: b :: t -> if a <= b then increases (b :: t) else false *)
  (* in *)
  updates
  (* |> filter (fun u -> u |> map (fun n -> IntMap.find n order_map) |> increases) *)
  |> map (fun u -> nth u ((length u - 1) / 2))
  |> fold_left ( + ) 0

let () =
  let lines = In_channel.input_lines (open_in file) in
  let new_line_i = find_index (fun line -> line = "") lines |> Option.get in

  let orders =
    filteri (fun i _ -> i < new_line_i) lines
    |> map (fun line ->
           match String.split_on_char '|' line |> map int_of_string with
           | [ i; j ] -> (i, j)
           | _ -> raise (Invalid_argument "this musnt happen"))
  in

  let updates =
    filteri (fun i _ -> i > new_line_i) lines
    |> map (fun line -> String.split_on_char ',' line |> map int_of_string)
  in

  print_int (part1 orders updates)
