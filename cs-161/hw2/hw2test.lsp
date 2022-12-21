(load "hw2.lsp")

(print (equal (BFS '((A (B)) C (D))) '(C A D B)))
(print (equal (BFS '(A (B C) (D) (E (F G)))) '(A B C D E F G)))
(print (equal (BFS '((A (B)) C ((D E) F G))) '(C A F G B D E)))

(print (equal (DFS '((A (B)) C (D))) '(D C B A)))
(print (equal (DFS '(A (B C) (D) (E (F G)))) '(G F E D C B A)))
(print (equal (DFS '((A (B)) C ((D E) F G))) '(G F E D C B A)))

(print (equal (DFID '((A (B)) C (D)) 3) '(C A C D A B C D)))
(print (equal (DFID '(A (B C) (D) (E (F G))) 3) '(A A B C D E A B C D E F G)))
(print (equal (DFID '((A (B C) D) E ((F))) 3) '(E A D E A B C D E F)))

(print (not (next-state '(3 3 t) 1 0)))
(print (not (next-state '(1 1 t) 2 0)))
(print (equal (next-state '(3 3 t) 0 1) '((0 1 NIL))))

(print (equal (succ-fn '(3 3 t)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))
(print (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))

(print (equal (mc-dfs '(3 3 NIL) NIL) '((3 3 NIL))))
(print (mc-dfs '(3 3 t) NIL))