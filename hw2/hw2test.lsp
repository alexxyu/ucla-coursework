(load "hw2.lsp")

(defun IS-SAME (E1 E2)
    (if (and (listp E1) (listp E2))
        (if (and (null E1) (null E2))
            T
            (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))
        )
        (if (and (not (listp E1)) (not (listp E2)))
            (equal E1 E2)
        )
    )
)

(print (IS-SAME (BFS '((A (B)) C (D))) '(C A D B)))
(print (IS-SAME (BFS '(A (B C) (D) (E (F G)))) '(A B C D E F G)))

(print (IS-SAME (DFS '((A (B)) C (D))) '(D C B A)))
(print (IS-SAME (DFS '(A (B C) (D) (E (F G)))) '(G F E D C B A)))
(print (IS-SAME (DFS '((A (B)) C ((D E) F G))) '(G F E D C B A)))

(print (IS-SAME (DFID '((A (B)) C (D)) 3) '(C A C D A B C D)))
(print (IS-SAME (DFID '(A (B C) (D) (E (F G))) 3) '(A A B C D E A B C D E F G)))

(print (not (next-state '(3 3 t) 1 0)))
(print (not (next-state '(1 1 t) 2 0)))
(print (IS-SAME (next-state '(3 3 t) 0 1) '((0 1 NIL))))
(print (IS-SAME (next-state '(3 3 t) 0 1) '((0 1 NIL))))

(print (IS-SAME (succ-fn '(3 3 t)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))
(print (IS-SAME (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))

(print (mc-dfs '(3 3 t) NIL))