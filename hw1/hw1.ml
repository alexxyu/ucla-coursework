type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Q1: Check if set a is a subset of set b *)
let rec subset a b =
    match a with
    [] -> true
    | x::tl -> (List.mem x b) && (subset tl b);;

(* Q2: Check if a and b are equal sets *)
let equal_sets a b = 
    (subset a b) = (subset b a);;

(* Q3: Find the union of set a and set b *)
let rec uniq a = 
    match a with
    [] -> []
    | x::tl ->  let set = (uniq tl) in
                if (List.mem x set) then set
                else x::set;;

let set_union a b =
    uniq (List.append a b);;

(* Q4: Find the union of all sets in a *)
let rec set_all_union a =
    match a with
    [] -> []
    | x::tl -> set_union x (set_all_union tl);;

(* Q5: Check whether set s is a member of itself *)
(* This is not possible to do because elements of a list in OCaml must be of the same type, and
   it is not clear what type such a set that contains itself would be *)

(* Q6: Return computed fixed point for f w.r.t. x with eq as equality predicate for f's domain*)
let rec computed_fixed_point eq f x =
    if (eq (f x) x) then x
    else computed_fixed_point eq f (f x);;

(* Q7: Return copy of grammar g with all unreachable rules removed *)
let get_rhs rule =
    match rule with (_, rhs) -> rhs;;

let get_lhs rule =
    match rule with (lhs, _) -> lhs;;

let rec get_nonterminal_symbols rhs =
    match rhs with
    [] -> []
    | T s::tl -> get_nonterminal_symbols tl
    | N s::tl -> s::(get_nonterminal_symbols tl);;

let filter_symbol_rules rules symbols = 
    List.filter (fun (lhs, _) -> List.mem lhs symbols) rules;;

let rec get_all_nonterminal_symbols rules =
    match rules with
    [] -> []
    | rule::tl -> set_union (get_nonterminal_symbols (get_rhs rule)) (get_all_nonterminal_symbols tl);;

let rec find_reachable_rules rules seen =
    let filtered_rules = filter_symbol_rules rules seen in
    let updated = set_union seen (get_all_nonterminal_symbols filtered_rules) in
    if (equal_sets updated seen) then updated
    else find_reachable_rules rules updated;;

let filter_reachable g =
    let (start_symbol, rules) = g in
    let reachable_symbols = find_reachable_rules rules [start_symbol] in
    start_symbol, (List.filter (fun rule -> List.mem (get_lhs rule) reachable_symbols) rules);;
