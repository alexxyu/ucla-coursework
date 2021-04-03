let my_subset_test0 = not (subset [1;2;3] [3;2;5;6])
let my_subset_test1 = not (subset [1;2;3] [])
let my_subset_test2 = not (subset [3;4] [3;7;8])
let my_subset_test3 = subset [3;4] [3;4;7;8]

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [[1;2];[3;4]] [[1;2];[3;4]]

let my_set_union_test0 = equal_sets (set_union [1;2;3] [3;2;1]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;2;3] []) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [[1;2];[3;4]] [[5;6]]) [[1;2];[3;4];[5;6]]
let my_set_union_test3 = equal_sets (set_union [1;2;3;4] [5;6;7;1;2]) [1;2;3;4;5;6;7]

let my_set_all_union_test0 = equal_sets (set_all_union [[1;2];[3;4];[5;6]]) [1;2;3;4;5;6]
let my_set_all_union_test1 = equal_sets (set_all_union [[1;2]]) [1;2]
let my_set_all_union_test2 = equal_sets (set_all_union [[1;2;3];[2;3;4]]) [1;2;3;4]

let my_computed_fixed_point_test0 = (computed_fixed_point (=) (fun x -> -(x*x)+42) 1000) = 6
let my_computed_fixed_point_test1 = (computed_fixed_point (=) (fun x -> 4*(x-6)) 64) = 8

type my_grammar_nonterminals = 
    | Sentence | Clause | Subject | Verb | Object | Conjunction

let my_grammar_rules =
    [Sentence, [N Sentence; N Conjunction; N Sentence];
     Sentence, [N Clause];
     Clause, [N Subject; N Verb];
     Clause, [N Subject; N Verb; N Object];
     Subject, [T"I"];
     Subject, [T"You"];
     Verb, [T"eat"];
     Verb, [T"say"];
     Verb, [T"want"];
     Object, [T"nothing"];
     Object, [T"everything"];
     Conjunction, [T"and"];
     Conjunction, [T"or"]
    ]

let my_filter_reachable_test0 = 
    filter_reachable (Sentence, my_grammar_rules) = (Sentence, my_grammar_rules)

let my_filter_reachable_test1 =
    filter_reachable (Clause, my_grammar_rules) =
        (Clause, [Clause, [N Subject; N Verb];
                  Clause, [N Subject; N Verb; N Object];
                  Subject, [T"I"];
                  Subject, [T"You"];
                  Verb, [T"eat"];
                  Verb, [T"say"];
                  Verb, [T"want"];
                  Object, [T"nothing"];
                  Object, [T"everything"]])

let my_filter_reachable_test2 =
    filter_reachable (Sentence, List.tl my_grammar_rules) = 
        (Sentence, [Sentence, [N Clause];
                    Clause, [N Subject; N Verb];
                    Clause, [N Subject; N Verb; N Object];
                    Subject, [T"I"];
                    Subject, [T"You"];
                    Verb, [T"eat"];
                    Verb, [T"say"];
                    Verb, [T"want"];
                    Object, [T"nothing"];
                    Object, [T"everything"]])

let my_filter_reachable_test3 =
    filter_reachable (Object, my_grammar_rules) = (Object, [Object, [T"nothing"];Object, [T"everything"]])