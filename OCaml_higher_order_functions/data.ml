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
  | IntNode (v,None,l,m,r) -> 
      if x = v then IntNode (v,None,l,m,r) 
      else if x > v then IntNode (v,Some x,l,m,r) 
      else IntNode (x,Some v,l,m,r)
  | IntNode (v1,Some v2,l,m,r) -> 
      if x = v1 || x = v2 then IntNode (v1,Some v2,l,m,r) 
      else if  x < v1 then IntNode (v1,Some v2,int_insert x l,m,r) 
      else if  x < v2 then IntNode (v1,Some v2,l,int_insert x m,r) 
      else IntNode (v1,Some v2,l,m,int_insert x r) 

let rec int_mem x t = match t with
  | IntLeaf -> false
  | IntNode (v,None,l,m,r) -> 
      if x = v then true
      else false
  | IntNode (v1,Some v2,l,m,r) -> 
      if x = v1 || x = v2 then true
      else if  x < v1 then int_mem x l
      else if  x < v2 then int_mem x m
      else int_mem x r

let rec int_size t = match t with
  | IntLeaf -> 0
  | IntNode (v,None,l,m,r) -> 1
  | IntNode (v1,Some v2,l,m,r) -> 2 + int_size l + int_size m + int_size r

let rec int_max t = match t with
  | IntLeaf -> raise( Invalid_argument "int_max")
  | IntNode (v,None,l,m,r) -> v
  | IntNode (v1,Some v2,l,m,r) -> 
      if r = IntLeaf then v2
      else int_max r

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with
  | MapLeaf -> MapNode ((k, v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((k1, v1),None,l,m,r) -> 
      if k1 = k then raise (Invalid_argument "map_put")
      else if k > k1 then MapNode ((k1,v1),Some (k, v),l,m,r) 
      else MapNode ((k,v),Some (k1,v1),l,m,r)
  | MapNode ((k1, v1),Some (k2, v2),l,m,r) -> 
      if k = k1 || k = k2 then raise (Invalid_argument "map_put")
      else if  k < k1 then MapNode ((k1, v1),Some (k2, v2),map_put k v l,m,r) 
      else if  k < k2 then MapNode ((k1, v1),Some (k2, v2),l,map_put k v m,r)
      else MapNode ((k1, v1),Some (k2, v2),l,m,map_put k v r)
  
let rec map_contains k t = match t with
  | MapLeaf -> false
  | MapNode ((k1, v1),None,l,m,r) -> 
      if k1 = k then true
      else false
  | MapNode ((k1, v1),Some (k2, v2),l,m,r) -> 
      if k = k1 || k = k2 then true
      else if  k < k1 then map_contains k l
      else if  k < k2 then map_contains k m
      else map_contains k r

let rec map_get k t =match t with
  | MapLeaf -> raise (Invalid_argument "map_get")
  | MapNode ((k1, v1),None,l,m,r) -> 
      if k1 = k then v1
      else raise (Invalid_argument "map_get")
  | MapNode ((k1, v1),Some (k2, v2),l,m,r) -> 
      if k = k1 then v1
      else if k = k2 then v2
      else if  k < k1 then map_get k l
      else if  k < k2 then map_get k m
      else map_get k r

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
  | Empty
  | Table of (((string * int) list)* lookup_table)

let empty_table : lookup_table = Empty

let push_scope (table : lookup_table) : lookup_table = 
  Table ([],table)

let pop_scope (table : lookup_table) : lookup_table = match table with
  | Empty -> failwith "No scopes remain!"
  | Table (_, t) -> t 

let add_var name value (table : lookup_table) : lookup_table = match table with
  | Empty -> failwith "There are no scopes to add a variable to!"
  | Table (m, t) -> 
      if (fold (fun b (n, _) -> (n = name) || b) false m) 
      then failwith "Duplicate variable binding in scope!"
      else Table ((name, value)::m, t)

let rec lookup name (table : lookup_table) = match table with
  | Empty -> failwith "Variable not found!"
  | Table (m, t) -> 
      if (fold (fun b (n, _) -> (n = name) || b) false m) 
      then fold (fun a (n, v) -> if n = name then v else a ) 0 m
      else lookup name t