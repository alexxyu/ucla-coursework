let reverse =
    let rec reverse_helper acc = function
        | [] -> acc
        | h::t -> reverse_helper (h::acc) t
    in
    reverse_helper;;

let mem e = List.exists ((=) e);;

let rec fold_right init op = function
    | [] -> init
    | head::rest -> op head (fold_right init op rest);;

let rec fold_left op acc = function
    | [] -> acc
    | head::rest -> fold_left op (op acc head) rest;;

let rec range n acc =
    match n with
    | 0 -> acc
    | _ -> range (n-1) (n::acc);;