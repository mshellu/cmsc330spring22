open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = match s with 
| None -> List.fold_left (fun acc x -> match x with |(a, None, c) -> if (Sets.elem a qs) then (Sets.insert c acc) else acc |_ -> acc) [] nfa.delta
| Some sym -> if (Sets.elem sym nfa.sigma) = true then (* if in sigma/finite alphabet *)
    (* go thru delta, for each element, if 'q is in qs && has transition s, then append to resulting list *)
    List.fold_left (fun acc x -> match x with |(a, Some b, c) -> if ((Sets.elem a qs) && b = sym) then (Sets.insert c acc) else acc |_ -> acc) [] nfa.delta
  else 
    [] (* s not in sigma *)

let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) (acc: 'q list) : 'q list = 
  let next_list = move nfa qs None in 
  (* if next list has repeat elems then remove them *)
  let next_list = Sets.diff next_list (Sets.intersection next_list acc) in 
  (* if no other states to epsilon trans to, return acc *)
  (* else if initial one transition, append qs and next_list (done in e_closure); else, append next_list *)
  (* acc is set of all elements you can get to with epsilon *)
  if next_list = [] then acc 
  else e_closure_helper nfa next_list (Sets.insert_all next_list acc)

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  e_closure_helper nfa qs (intersection qs nfa.qs) (* passing in only given states that exist in nfa *)

let rec accept_helper (nfa: ('q,char) nfa_t) (qs: 'q list) (char_list: char list) : bool = 
  match char_list with 
  | [] -> if (Sets.intersection nfa.fs qs) = [] then false else true (* reached end means all letters passed, check if final state(s) in qs *)
  | h::t -> if qs = [] then false else accept_helper nfa (e_closure nfa (move nfa qs (Some h))) t

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let char_list = explode s in
  (* first step: e_closure to get starting states *)
  (* while haven't reached end of string, do 1. move based on next char then 2. e_closure to find next states *)
  (* if last possible states contain final state, return true *)
  accept_helper nfa (e_closure nfa [nfa.q0]) char_list

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  (* find list of states for each item in sigma (alphabet) from single transition then epsilon *)
  (* so answer list is length of sigma. here 'a' represents an item in sigma *)
  List.map (fun a -> e_closure nfa (move nfa qs (Some a))) nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.map (fun a -> (qs, Some a, e_closure nfa (move nfa (e_closure nfa qs) (Some a)))) nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (Sets.intersection nfa.fs qs) = [] then [] else [qs]

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = 
  match work with 
  | [] -> dfa
  | h::t -> if (Sets.elem h dfa.qs) || h = [] then nfa_to_dfa_step nfa dfa t (* if item is [] or already processed then ignore *)
    else nfa_to_dfa_step nfa 
      {sigma= dfa.sigma; 
      qs= h::dfa.qs;
      q0= dfa.q0; 
      fs= (new_finals nfa h)@dfa.fs; 
      delta= (new_trans nfa h)@dfa.delta} (t@(new_states nfa h))

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let start = e_closure nfa [nfa.q0] in 
  nfa_to_dfa_step nfa {sigma= nfa.sigma; qs= []; q0= start; fs= []; delta= []} [start]