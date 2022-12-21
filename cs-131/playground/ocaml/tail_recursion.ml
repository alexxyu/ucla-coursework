let rec fact = function
    | 0 -> 1
    | x -> x*(fact (x-1));;

let fact_with_acc n =
    let rec fact_helper acc = function
        | 0 -> acc
        | x -> fact_helper (x*acc) (x-1)
    in
    fact_helper 1 n;;

let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx;;

let t1 = time fact 100000;;
let t2 = time fact_with_acc 100000;;