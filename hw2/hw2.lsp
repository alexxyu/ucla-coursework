(defun BFS (TREE)
    (if (null TREE)
        NIL
        (if (atom (car TREE))
            (cons (car TREE) (BFS (cdr TREE)))
            (BFS (append (cdr TREE) (car TREE)))
        )
    )
)

(defun DFS (TREE)
    (if (null TREE)
        NIL
        (if (atom (car TREE))
            (append (DFS (cdr TREE)) (list (car TREE)))
            (append (DFS (cdr TREE)) (DFS (car TREE)))
        )
    )
)

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

(defun DFID (TREE M)
    (if (= M 0)
        NIL
        (append (DFID TREE (- M 1)) (DFID-SEARCH TREE 0 M))
    )
)

(print (BFS '((A (B)) C (D))))
(print (BFS '(A (B C) (D) (E (F G)))))

(print (DFS '((A (B)) C (D))))
(print (DFS '(A (B C) (D) (E (F G)))))
(print (DFS '((A (B)) C ((D E) F G))))

(print (DFID '((A (B)) C (D)) 3))
(print (DFID '(A (B C) (D) (E (F G))) 3))