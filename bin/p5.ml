let file = "data/p5.txt"

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
open List

type graph = Node of (int * graph ref t) | Leaf of int

let num_in_nodes num nodes =
  exists
    (fun node -> match !node with Node (n, _) -> n = num | Leaf n -> n = num)
    nodes

let node_num = function Node (n, _) -> n | Leaf n -> n

let order_scores orders nums =
  let node_table = Hashtbl.create 123456 in
  let orders = filter (fun (a, b) -> mem a nums && mem b nums) orders in
  let root_nums =
    map (fun (before, _) -> before) orders |> IntSet.of_list |> ref
  in

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

      before_node :=
        match !before_node with
        | Node (_, children) ->
            if num_in_nodes before children then Node (before, children)
            else Node (before, after_node :: children)
        | Leaf _ -> Node (before, [ after_node ]))
    orders;

  let parents_table = Hashtbl.create 123455 in

  let update_parents num parents =
    Hashtbl.replace parents_table num
      (IntSet.union parents
         (Option.value
            (Hashtbl.find_opt parents_table num)
            ~default:IntSet.empty))
  in

  IntSet.iter
    (fun root_num ->
      let visited = ref IntSet.empty in
      let rec score_node parents node =
        if not (IntSet.mem (node_num !node) !visited) then (
          match !node with
          | Node (n, children) ->
              visited := IntSet.add n !visited;
              update_parents n parents;
              iter (fun cn -> score_node (IntSet.add n parents) cn) children
          | Leaf n ->
              visited := IntSet.add n !visited;
              update_parents n parents)
        else ()
      in
      score_node IntSet.empty (Hashtbl.find node_table root_num))
    !root_nums;

  (* Format.printf "%d\n" (IntSet.to_list !root_nums |> length); *)
  parents_table

let part1 orders updates =
  let is_valid update =
    let parents = order_scores orders update in
    let rec aux invalid = function
      | [] -> true
      | num :: t ->
          if IntSet.mem num invalid then false
          else
            let p =
              Option.value (Hashtbl.find_opt parents num) ~default:IntSet.empty
            in
            aux (IntSet.union invalid p) t
    in
    aux IntSet.empty update
  in

  updates |> filter is_valid
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
