let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type my_grammar_nonterminals = 
    | Amount | Dollars | Cents | Digit
    
let my_grammar_rules =
    [Amount, [T"$"; N Dollars];
     Amount, [T"$"; N Dollars; T"."; N Cents];
     Dollars, [N Digit; N Dollars];
     Dollars, [N Digit];
     Cents, [N Digit; N Digit];
     Digit, [T"0"];
     Digit, [T"1"];
     Digit, [T"2"];
     Digit, [T"3"];
     Digit, [T"4"];
     Digit, [T"5"];
     Digit, [T"6"];
     Digit, [T"7"];
     Digit, [T"8"];
     Digit, [T"9"]]
 
let my_simple_frag = ["$"; "1"; "2"; "."; "3"; "4"]
let my_grammar_prod_func = (function (_, rhs) -> rhs) (convert_grammar (Amount, my_grammar_rules))
let my_grammar_matcher = make_matcher (Amount, my_grammar_prod_func)
let my_grammar_parser = (make_parser (Amount, my_grammar_prod_func))

(* convert_grammar tests *)
let test0 = ((my_grammar_prod_func Digit) = 
    [[T"0"];[T"1"];[T"2"];[T"3"];[T"4"];[T"5"];[T"6"];[T"7"];[T"8"];[T"9"]])
let test1 = ((my_grammar_prod_func Amount) = 
    [[T"$"; N Dollars];[T"$"; N Dollars; T"."; N Cents]])

(* parse_tree_leaves tests *)
let test2 =
    (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])])) = [3; 4; 5])
let test3 =
    (parse_tree_leaves (Node ("+", [Node ("+", [Leaf 1; Node ("-", [Leaf 2; Leaf 3])]); Node ("*", [Leaf 4; Leaf 5])])) = [1; 2; 3; 4; 5])

(* make_matcher tests *)
let test4 = ((my_grammar_matcher accept_empty_suffix ["$"; "1"; "2"; "3"; "."; "4"; "5"]) = Some [])
let test5 = ((my_grammar_matcher accept_all ["$"; "1"; "2"; "3"; "."; "4"; "5"]) = Some ["."; "4"; "5"])
let test6 = ((my_grammar_matcher accept_empty_suffix ["$"; "1"; "2"; "3"; "."]) = None)

(* make_parser tests *)
let test7 = (my_grammar_parser my_simple_frag =
    Some(Node (Amount, 
            [Leaf "$"; 
             Node (Dollars, 
                [Node (Digit,
                    [Leaf "1"]);
                 Node (Dollars,
                    [Node (Digit,
                        [Leaf "2"])])]);
             Leaf ".";
             Node (Cents,
                [Node (Digit, 
                    [Leaf "3"]);
                 Node (Digit,
                    [Leaf "4"])])])))

let test8 =
    match my_grammar_parser my_simple_frag with
        | Some tree -> parse_tree_leaves tree = my_simple_frag
        | _ -> false