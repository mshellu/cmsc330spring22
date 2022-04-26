open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = match t with
| IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
| IntNode (a, b, c, d, e) -> match a, b, c, d, e with
  | a, None, c, d, e -> 
      if x > a then IntNode (a, Some x, c, d, e) 
      else if x < a then IntNode(x, Some a, c, d, e)
      (* no duplicates *)
      else t
  | a, Some b, c, d, e -> 
      if x < a then IntNode (a, Some b, int_insert x c, d, e) 
      else if x > a && x < b then IntNode (a, Some b, c, int_insert x d, e)
      else if x > b then IntNode (a, Some b, c, d, int_insert x e) 
      (* last else means x = a or x = b *)
      else t

let rec int_mem x t = match t with
| IntLeaf -> false
| IntNode (a, b, c, d, e) -> if a = x || Some x = b then true 
    else if x < a then int_mem x c (* recursively call left subtree *)
    else if x > a && Some x < b then int_mem x d 
    else int_mem x e

let rec int_size t = match t with
| IntLeaf -> 0
(* 1 for a, check if Some b is there, int_size subtrees *)
| IntNode (a, b, c, d, e) -> if b <> None then 2 + int_size c + int_size d + int_size e 
    else 1 + int_size c + int_size d + int_size e

(* cases: has b and no e, return b; has b and has e, recurse e; no b and no e = a *)
let rec int_max t = match t with 
| IntNode (a, None, c, d, e) -> a
| IntNode (a, Some b, c, d, e) -> 
    if e = IntLeaf then b (* has b and no e *)
    else int_max e (* has b and has e *)
| IntLeaf -> raise (Invalid_argument("int_max"))

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with
| MapLeaf -> MapNode ((k, v), None, MapLeaf, MapLeaf, MapLeaf)
| MapNode ((a, av), None, c, d, e) -> 
    if k > a then MapNode ((a, av), Some (k, v), c, d, e) 
    else if k < a then MapNode ((k, v), Some (a, av), c, d, e)
    else raise (Invalid_argument("map_put"))
| MapNode ((a, av), Some (b, bv), c, d, e) -> 
    if k < a then MapNode ((a, av), Some (b, bv), map_put k v c, d, e) 
    else if k > a && k < b then MapNode ((a, av), Some (b, bv), c, map_put k v d, e)
    else if k > b then MapNode ((a, av), Some (b, bv), c, d, map_put k v e) 
    else raise (Invalid_argument("map_put"))

let rec map_contains k t = match t with
| MapLeaf -> false
| MapNode ((a, av), Some (b, bv), c, d, e) -> if a = k || b = k then true 
    else if k < a then map_contains k c 
    else if k > a && k < b then map_contains k d 
    else map_contains k e
| MapNode ((a, av), None, c, d, e) -> if a = k then true else false

let rec map_get k t = match t with
| MapLeaf -> raise (Invalid_argument("map_get"))
| MapNode ((a, av), Some (b, bv), c, d, e) -> 
    if a = k then av 
    else if b = k then bv   
    else if k < a then map_get k c 
    else if k > a && k < b then map_get k d 
    else map_get k e
| MapNode ((a, av), None, c, d, e) -> if a = k then av else raise (Invalid_argument("map_get"))

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
  | EmptyList
  | List of ((string * int) list) * lookup_table 

let empty_table : lookup_table = EmptyList
(* list looks like ([(a, 3); (b, 4)], ([(a, 2); (b, 5)], ...)) with outer element being most recent scope *)

let push_scope (table : lookup_table) : lookup_table = match table with 
| EmptyList -> List ([], EmptyList)
(* a is [(a, 3); (b, 4)], b is in (a, b) as in (a, (list of string*int, lookup_table))*)
| List (a, b) -> List ([], table)

let pop_scope (table : lookup_table) : lookup_table = match table with 
| EmptyList -> failwith "No scopes remain!"
| List (a, b) -> b

(* helper method from higher.ml but different *)
let contains_elem lst e = fold (fun a b -> a || match b with | (c, d) -> c = e) false lst

let add_var name value (table : lookup_table) : lookup_table = match table with 
| EmptyList -> failwith "There are no scopes to add a variable to!"
| List (a, b) -> 
    if contains_elem a name then failwith "Duplicate variable binding in scope!" 
    else List ((name, value) :: a, b)

let rec lookup name (table : lookup_table) = match table with 
| EmptyList -> failwith "Variable not found!"
| List (a, b) -> match a with (* look inside list a *)
  | [] -> failwith "Variable not found!"
  | h::t -> match h with (c, d) -> if c = name then d else lookup name (List (t, b))