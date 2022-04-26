open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a b -> a || (b = e)) false lst

let is_present lst x = map (fun a -> if a = x then 1 else 0) lst

let count_occ lst target = fold (fun a b -> if b = target then a + 1 else a) 0 lst

(* a represents final list *)
let uniq lst = fold (fun a x -> if (contains_elem a x = true) then a else x::a) [] lst

let assoc_list lst = fold (fun a x -> if not (contains_elem a (x, count_occ lst x)) then (x, count_occ lst x)::a else a) [] lst
(* f is function in fns, a is final array *)
let ap fns args = fold (fun a f -> a @ (map f args)) [] fns
