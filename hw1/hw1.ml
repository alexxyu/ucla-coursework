type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* T1: Check if set a is a subset of set b *)
let rec subset a b =
    match a with
    [] -> true
    | x::tl -> (List.mem x b) && (subset tl b);;

(* T2: Check if a and b are equal sets *)
let equal_sets a b = 
    (subset a b) = (subset b a);;

(* T3: Find the union of set a and set b *)
let rec uniq a = 
    match a with
    [] -> []
    | x::tl ->  let set = (uniq tl) in
                if (List.mem x set) then set
                else x::set;;

let set_union a b =
    uniq (List.append a b);;

(* T4: Find the union of all sets in a *)
let rec set_all_union a =
    match a with
    [] -> []
    | x::tl -> set_union x (set_all_union tl);;

(* T5: Check whether set s is a member of itself *)
(* This is not possible to do because elements of a list in OCaml must be of the same type, and
   it is not clear what type such a set that contains itself would be *)

(* T6: Return computed fixed point for f w.r.t. x with eq as equality predicate for f's domain*)
let rec computed_fixed_point eq f x =
    if (eq (f x) x) then x
    else computed_fixed_point eq f (f x);;

(* T7: Return copy of grammar g with all unreachable rules removed *)
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

(* Test cases and helper print functions *)
let print_bool b = 
    if b then print_string "true"
    else print_string "false";;

let rec print_list a = 
    match a with
    [] -> ()
    | e::l -> print_int e ; print_string " " ; print_list l;;

print_bool (subset [3;4] [3;7;8]);;
print_newline ();;
print_bool (subset [3;4] [3;4;7;8]);;
print_newline ();;

let a = [1;2;3;4];;
let b = [5;6;7;1;2];;
let c = set_union a b;;
print_list c;;
print_newline ();;

let k = [[1;2;3];[2;3]];;
let m = set_all_union k;;
print_list m;;
print_newline ();;

print_int (computed_fixed_point (=) (fun x -> x * x) 100);;
print_newline ();;