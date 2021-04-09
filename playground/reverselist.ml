let reverse =
    let rec reverse_helper acc = function
        | [] -> acc
        | h::t -> reverse_helper (h::acc) t
    in
    reverse_helper;;