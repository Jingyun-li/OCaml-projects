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

let move_aux1 q s t l =
  let (a, b, c) = t in
  if (a = q) && (b = s) then insert c l else l
    
let move_aux2 q s delta lst =
  List.fold_right (fun t l -> move_aux1 q s t l) delta lst

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  List.fold_right(fun e l -> move_aux2 e s nfa.delta l) qs [] 
  
let e_closure_aux2 q t l =
  let (a, b, c) = t in
  if (a = q) && (b = None) then insert c l else l
      
let e_closure_aux1 q delta lst =
  List.fold_right (fun t l -> e_closure_aux2 q t l) delta lst
    
let rec e_closure_aux0 delta qs =
  let l0 = qs in
  let l1 = List.fold_right (fun q lst -> e_closure_aux1 q delta lst) qs qs in
  if List.length l0 = List.length l1 then l1
  else e_closure_aux0 delta l1
  
let e_closure (nfa: ('q, 's) nfa_t) (qs: 'q list) : 'q list =
  e_closure_aux0 nfa.delta qs

let rec accept_aux1 nfa qs charlst = 
  match charlst with
  | [] -> e_closure nfa qs 
  | h::t -> let lst = move nfa (e_closure nfa qs) (Some h) in 
      accept_aux1 nfa lst t
  
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let charlst = explode s in
  let lst = accept_aux1 nfa [nfa.q0] charlst in
  let lst1 = intersection nfa.fs lst in
  List.length lst1 > 0 

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun l e -> insert (e_closure nfa (move nfa qs (Some e))) l) [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun l e -> insert (qs, Some e, (e_closure nfa (move nfa qs (Some e)))) l) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun l e -> if elem e qs then insert qs l else l) [] nfa.fs

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = match work with
  | [] -> dfa
  | h::t -> let new_dfa = {
      sigma = nfa.sigma;
      qs = union (new_states nfa h) dfa.qs;
      q0 = dfa.q0;
      fs = union (new_finals nfa h) dfa.fs;
      delta = union (new_trans nfa h) dfa.delta
    } in nfa_to_dfa_step nfa new_dfa t 
        
let rec nfa_to_dfa_aux nfa dfa work = 
  let old = dfa.qs in
  let new_dfa = nfa_to_dfa_step nfa dfa work in 
  let new_work = diff new_dfa.qs old in
  if List.length work = 0 then new_dfa else nfa_to_dfa_aux nfa new_dfa new_work
    
let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa_q0 = e_closure nfa [nfa.q0] in
  let dfa = {
    sigma = nfa.sigma;
    qs = new_states nfa dfa_q0;
    q0 = dfa_q0;
    fs = new_finals nfa dfa_q0;
    delta = new_trans nfa dfa_q0
  } in nfa_to_dfa_aux nfa dfa dfa.qs
