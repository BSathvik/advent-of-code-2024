let file = "data/p5.txt"

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
open List

(* type order = (int, order) hashtbl.t | leaf of int *)

type graph = Node of (int * graph ref t) | Leaf of int

let num_in_nodes num nodes =
  exists
    (fun node -> match !node with Node (n, _) -> n = num | Leaf n -> n = num)
    nodes

let node_num = function Node (n, _) -> n | Leaf n -> n

let order_scores orders =
  let node_table = Hashtbl.create 123456 in
  let root_nums =
    ref (map (fun (before, _) -> before) orders |> IntSet.of_list)
  in

  Format.printf "init root_nums: %d\n" (IntSet.to_list !root_nums |> length);
  Format.printf "init orders: %d\n" (length orders);

  List.iter
    (fun (before, after) ->
      let before_node =
        match Hashtbl.find_opt node_table before with
        | Some node -> node
        | None ->
            let node = ref (Node (before, [])) in
            Hashtbl.replace node_table before node;
            node
      in
      let after_node =
        match Hashtbl.find_opt node_table after with
        | Some node -> node
        | None ->
            let node = ref (Leaf after) in
            Hashtbl.replace node_table after node;
            node
      in

      (* `after` num cannot be a candidate for the root node *)
      root_nums := IntSet.remove after !root_nums;

      (* print_int (IntSet.to_list !root_nums |> length); *)
      before_node :=
        match !before_node with
        | Node (_, children) ->
            if num_in_nodes before children then Node (before, children)
            else Node (before, after_node :: children)
        | Leaf _ -> Node (before, [ after_node ]))
    orders;

  let score_table = Hashtbl.create 123455 in

  let update_score num score =
    Hashtbl.replace score_table num
      (max (Option.value (Hashtbl.find_opt score_table num) ~default:0) score)
  in

  Hashtbl.iter
    (fun _ node ->
      let visited = ref IntSet.empty in

      let rec score_node score node =
        if not (IntSet.mem (node_num !node) !visited) then (
          match !node with
          | Node (n, children) ->
              visited := IntSet.add n !visited;
              update_score n score;
              iter (fun cn -> score_node (score + 1) cn) children
          | Leaf n ->
              visited := IntSet.add n !visited;
              update_score n score)
        else ()
      in
      score_node 0 node)
    node_table;

  Format.printf "final root_nums: %d\n" (IntSet.to_list !root_nums |> length);
  Format.printf "final orders: %d\n" (length orders);
  Hashtbl.iter (fun k v -> Format.printf "%d: %d\n" k v) score_table;
  Format.printf "final orders: %d\n" (length orders);
  score_table

let part1 orders updates =
  (* iter (fun (i, j) -> Format.printf "%d|%d\n" i j) orders; *)
  (* iter *)
  (*   (fun l -> String.concat "," (map string_of_int l) |> print_endline) *)
  (*   updates; *)
  let scores = order_scores orders in

  let rec increases = function
    | [] | [ _ ] -> true
    | a :: b :: t -> if a <= b then increases (b :: t) else false
  in

  updates
  |> filter (fun u -> u |> map (fun n -> Hashtbl.find scores n) |> increases)
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

  Format.printf "part1: %d\n" (part1 orders updates)
