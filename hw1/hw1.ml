(* T1: Check if set a is a subset of set b *)
let rec subset a b =
    match a with
    [] -> true
    | x::tl -> (List.mem x b) && (subset tl b);;

(* T2: Check if a and b are equal sets *)
let equal_sets a b = 
    (subset a b) = (subset b a);;

(* T3: Find the union of set a and set b *)
let rec set_union a b =
    match a with
    [] -> b
    | x::tl -> x::(set_union tl b);;

(* T4: Find the union of all sets in a *)
let rec set_all_union a =
    match a with
    [] -> []
    | x::tl -> set_union x (set_all_union tl);;

(* T5: Check whether set s is a member of itself *)

(* T6: Return computed fixed point for f w.r.t. x with eq as equality predicate for f's domain*)

(* T7: Return copy of grammar g with all unreachable rules removed *)

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
let b = [5;6;7];;
let c = set_union a b;;
print_list c;;
print_newline ();;

let k = [[1;2;3];[2;3]];;
let m = set_all_union k;;
print_list m;;
print_newline ();;
