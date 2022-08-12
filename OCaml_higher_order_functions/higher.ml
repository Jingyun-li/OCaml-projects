open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
  fold (fun a b -> if b = e then (a||true) else a) false lst

let is_present lst x = 
  map (fun y -> if x = y then 1 else 0) lst

let count_occ lst target =
  fold (fun a b -> a+b) 0 (map (fun y -> if target = y then 1 else 0) lst)

let uniq lst = 
  fold (fun a b -> if contains_elem a b then a else b::a) [] lst

let assoc_list lst = 
  map (fun a -> (a, count_occ lst a)) (uniq lst)

let ap fns args =
  fold_right(fun f a -> fold_right (fun e b -> (f e)::b) args a) fns []