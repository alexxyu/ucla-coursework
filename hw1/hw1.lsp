(defun TREE-CONTAINS (N TREE)
    (if (null TREE)
        NIL
        (if (listp TREE)
            (if (> (second TREE) N)
                (TREE-CONTAINS N (first TREE))
                (TREE-CONTAINS N (third TREE))
            )
            (if (= N TREE)
                T
                NIL
            )
        )
    )
)

(defun TREE-MIN (TREE)
    (if (not (listp TREE))
        TREE
        (TREE-MIN (first TREE))
    )
)

(defun TREE-ORDER (TREE)
    (if (not (null TREE))
        (if (not (listp TREE)) 
            (list TREE)
            (append (cons (second TREE) (TREE-ORDER (first TREE))) (TREE-ORDER (third TREE)))
        )
    )
)

(print "TREE-CONTAINS TESTS")
(print (TREE-CONTAINS 3 3))
(print (TREE-CONTAINS 3 '(1 2 3)))
(print (TREE-CONTAINS 3 '((1 2 3) 7 8)))
(print (TREE-CONTAINS 3 '((1 2 3) 5 (6 8 (9 10 (11 12 13))))))

(print (TREE-CONTAINS 5 3))
(print (TREE-CONTAINS 5 '(1 2 3)))
(print (TREE-CONTAINS 5 '((1 2 3) 7 8)))
(print (TREE-CONTAINS 333 '((1 2 3) 5 (6 8 (9 10 (11 12 13))))))

(print "TREE-MIN TESTS")
(print (TREE-MIN 3))
(print (TREE-MIN '((1 2 3) 7 8)))

(print "TREE-ORDER TESTS")
(print (TREE-ORDER 3))
(print (TREE-ORDER '((1 2 3) 7 8)))
