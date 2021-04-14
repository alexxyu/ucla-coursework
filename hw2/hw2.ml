type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Q1: Convert HW1-style grammar to HW2-style grammar *)
(* https://stackoverflow.com/questions/46883375/dynamically-generating-a-function-with-pattern-matching-in-ocaml *)
let rec generate_alternative_lists rules = function
    | [] -> []
    | curr_symbol::t -> 
        let symbol_filter symbol = function
            | (lhs, _) -> lhs = symbol 
        in
        let symbol_rules = 
            List.map (function (_, rhs) -> rhs) (List.filter (symbol_filter curr_symbol) rules)
        in
        (curr_symbol, symbol_rules)::(generate_alternative_lists rules t);;

let key_to_value mapping key =
    List.assoc key mapping;;

let rec get_nonterminal_symbols = function
    | [] -> []
    | h::t -> 
        let nonterminal_symbols = (get_nonterminal_symbols t) in
        match h with
        | (lhs, _) when (List.mem lhs nonterminal_symbols) -> nonterminal_symbols
        | (lhs, _) -> lhs::nonterminal_symbols;;

let rec convert_grammar gram1 = 
    match gram1 with
    | (start_symbol, rules) -> 
        let mapping = 
            generate_alternative_lists rules (get_nonterminal_symbols rules)
        in
        start_symbol, (key_to_value mapping);;

(* Q2: Return list of leaves from left-to-right traversal of parse tree *)
let rec search_subtree = function
    | [] -> []
    | (Node (_, subtree))::t -> List.append (search_subtree subtree) (search_subtree t)
    | (Leaf leaf)::t -> leaf::(search_subtree t);;

let parse_tree_leaves tree =
    match tree with
    | Leaf leaf -> [leaf]
    | Node (_, subtree) -> search_subtree subtree;;

(* Q3: Return matcher for the given grammar *)
let rec apply_rule prod_func accept rule frag = 
    match rule with
    | [] -> accept frag
    | h::t -> 
        match h with
        | N nonterm when frag != [] -> 
            (* backtracked_accept allows us to continue matching the current rule to the remaining fragment after this 
               nonterminal symbol's rules have been matched *)
            let backtracked_accept = (apply_rule prod_func accept t) in
            apply_rules prod_func backtracked_accept (prod_func nonterm) frag
        | T term when (frag != [] && term = (List.hd frag)) -> apply_rule prod_func accept t (List.tl frag)
        | _ -> None

and apply_rules prod_func accept rules frag =
    match rules with
    | [] -> None
    | h::t ->
        match (apply_rule prod_func accept h frag) with
        | None -> apply_rules prod_func accept t frag
        | Some a -> Some a;;

let make_matcher gram =
    match gram with
    | (start, prod_func) -> (fun accept frag -> apply_rules prod_func accept (prod_func start) frag);;

(* Q4: Return parser for the given grammar *)
let accept_parse_tree tree frag =
    match frag with
    | [] -> Some tree
    | _ -> None;;

let rec parse_with_rule prod_func accept rule parent children frag = 
    match rule with
    | [] -> accept (Node (parent, children)) frag
    | h::t -> 
        match h with
        | N nonterm when frag != [] -> 
            (* backtracked_accept allows us to continue matching the current rule to the remaining fragment after this 
               nonterminal symbol's rules have been matched *)
            let backtracked_accept subtree = (parse_with_rule prod_func accept t parent (children@[subtree])) in
            parse_with_rules prod_func backtracked_accept (prod_func nonterm) nonterm frag
        | T term when (frag != [] && term = (List.hd frag)) -> 
            let updated_children = children @ [Leaf term] in
            parse_with_rule prod_func accept t parent updated_children (List.tl frag)
        | _ -> None

and parse_with_rules prod_func accept rules parent frag =
    match rules with
    | [] -> None
    | h::t ->
        match (parse_with_rule prod_func accept h parent [] frag) with
        | None -> parse_with_rules prod_func accept t parent frag
        | Some a -> Some a;;

let make_parser gram = 
    match gram with
    | (start, prod_func) -> (fun frag -> parse_with_rules prod_func accept_parse_tree (prod_func start) start frag);;