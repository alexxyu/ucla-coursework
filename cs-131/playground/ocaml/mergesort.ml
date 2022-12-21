let rec merge_sort list =
    match list with
    | [] -> []
    | [a] -> [a]
    | list -> 
        let rec halve l =
            match l with
            | [] -> ([], [])
            | [a] -> ([a], [])
            | a::b::rest ->   let (x, y) = halve rest in
                                (a::x, b::y)
        in
        let rec merge (a, b) =
            match (a, b) with
            | ([], b) -> b
            | (a, []) -> a
            | (x::restA, y::restB) -> 
            if x < y then x::(merge (restA, b))
            else y::(merge (a, restB))
        in
        let (x, y) = halve list in
        merge (merge_sort x, merge_sort y);;