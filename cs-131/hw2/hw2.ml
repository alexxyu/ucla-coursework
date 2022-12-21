type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Q1: Convert HW1-style grammar to HW2-style grammar *)
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
        start_symbol, (fun symbol -> List.assoc symbol mapping);;

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
let rec parse_with_rule prod_func accept rule parent children frag = 
    match rule with
    | [] -> accept (Node (parent, children)) frag
    | h::t -> 
        match h with
        | N nonterm when frag != [] -> 
            (* chained_parse allows us to continue matching the current rule to the remaining fragment after this 
               nonterminal symbol's rules have been matched *)
            let 
                chained_parse subtree = (parse_with_rule prod_func accept t parent (children @ [subtree])) 
            in
            parse_with_rules prod_func chained_parse (prod_func nonterm) nonterm frag
        | T term when (frag != [] && term = (List.hd frag)) -> 
            parse_with_rule prod_func accept t parent (children @ [Leaf term]) (List.tl frag)
        | _ -> None

and parse_with_rules prod_func accept rules parent frag =
    match rules with
    | [] -> None
    | h::t ->
        match (parse_with_rule prod_func accept h parent [] frag) with
        | None -> parse_with_rules prod_func accept t parent frag
        | Some a -> Some a;;

let make_matcher gram =
    match gram with
    | (start, prod_func) -> 
        fun accept frag -> 
            let accept_wrapper unused frag = accept frag in
            parse_with_rules prod_func accept_wrapper (prod_func start) start frag;;

(* Q4: Return parser for the given grammar *)
let accept_parse_tree tree frag =
    match frag with
    | [] -> Some tree
    | _ -> None;;

let make_parser gram = 
    match gram with
    | (start, prod_func) -> 
        fun frag -> parse_with_rules prod_func accept_parse_tree (prod_func start) start frag;;