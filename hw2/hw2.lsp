; Q1: BFS
; ARGUMENTS: the list representation of a tree TREE
; RETURNS: a single, top-level list of the terminal nodes in TREE in the order they would be visited by a left-to-right 
; breadth-first search
; Treats the list representation as a queue by appending newly seen subtrees to the end for processing
(defun BFS (TREE)
    (if (null TREE)
        NIL
        (if (atom (car TREE))
            (cons (car TREE) (BFS (cdr TREE)))
            (BFS (append (cdr TREE) (car TREE)))
        )
    )
)

; Q2: DFS
; ARGUMENTS: the list representation of a tree TREE
; RETURNS: a single, top-level list of the terminal nodes in TREE in the order they would be visited by a right-to-left 
; depth-first search
; Treats the list representation as a stack by visiting newly seen subtrees first, and also reverses the order of 
; appending to the list to account for right-to-left search
(defun DFS (TREE)
    (if (null TREE)
        NIL
        (if (atom (car TREE))
            (append (DFS (cdr TREE)) (list (car TREE)))
            (append (DFS (cdr TREE)) (DFS (car TREE)))
        )
    )
)

; DFID-SEARCH
; ARGUMENTS: the list representation of a tree TREE, an integer representing the max depth of the tree, M, and
;   and integer representing the current depth of the search, D
; RETURNS: a single top-level list of the terminal nodes in TREE in the order that they would be visited by a 
;   left-to-right depth-first search limited up to depth M
; Helper function that works very much like the DFS function but stops when 
(defun DFID-SEARCH (TREE D M)
    (if (>= D M) 
        NIL
        (if (null TREE)
            NIL    
            (if (atom (car TREE))
                (cons (car TREE) (DFID-SEARCH (cdr TREE) D M))
                (append (DFID-SEARCH (car TREE) (+ D 1) M) (DFID-SEARCH (cdr TREE) D M))
            )
        )
    )
)

; Q3: DFID
; ARGUMENTS: the list representation of a tree TREE, and an integer representing the max depth of the tree, M
; RETURNS: a single top-level list of the terminal nodes in TREE in the order that they would be visited by a 
; left-to-right depth-first iterative-deepening search
; Recursively calls and concats the output of DFID-SEARCH with maximum depth values from 0 to M
(defun DFID (TREE M)
    (if (= M 0)
        NIL
        (append (DFID TREE (- M 1)) (DFID-SEARCH TREE 0 M))
    )
)