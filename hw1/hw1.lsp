; Q1
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

; Q2
(defun TREE-MIN (TREE)
    (if (not (listp TREE))
        TREE
        (TREE-MIN (first TREE))
    )
)

; Q3
(defun TREE-ORDER (TREE)
    (if (not (null TREE))
        (if (not (listp TREE)) 
            (list TREE)
            (append (cons (second TREE) (TREE-ORDER (first TREE))) (TREE-ORDER (third TREE)))
        )
    )
)

; Q4
(defun SUB-LIST (L START LEN)
    (if (> START 0)
        (SUB-LIST (rest L) (- START 1) LEN)
        (if (> LEN 0)
            (cons (car L) (SUB-LIST (rest L) START (- LEN 1)))
            NIL
        )
    )
)

; Q5
(defun SPLIT-LIST (L)
    (let ((x (ceiling (/ (length L) 2))))
        (if (oddp (length L))
            (list (SUB-LIST L 0 x) (SUB-LIST L x (- x 1)))
            (list (SUB-LIST L 0 x) (SUB-LIST L x x))
        )
    )
)

; Q6
(defun BTREE-HEIGHT (TREE)
    (if (or (atom TREE) (null TREE))
        0
        (+ 1 (max (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (cadr TREE))))
    )
)

; Q7
(defun LIST2BTREE (LEAVES)
    (if (<= (length LEAVES) 1)
        (car LEAVES)
        (let ((split (SPLIT-LIST LEAVES)))
            (list (LIST2BTREE (first split)) (LIST2BTREE (second split)))
        )
    )
)

; Q8
(defun BTREE2LIST (TREE)
    (if (not (listp TREE))
        (list TREE)
        (if (null TREE)
            NIL
            (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE)))
        )
    )
)

; Q9
(defun IS-SAME (E1 E2)
    (if (and (listp E1) (listp E2))
        (if (and (null E1) (null E2))
            T
            (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))
        )
        (if (and (not (listp E1)) (not (listp E2)))
            (= E1 E2)
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

(print "SUB-LIST TESTS")
(print (SUB-LIST '(a b c d) 0 3))
(print (SUB-LIST '(a b c d) 3 1))
(print (SUB-LIST '(a b c d) 2 0))

(print "SPLIT-LIST TESTS")
(print (SPLIT-LIST '(a b c d)))
(print (SPLIT-LIST '(a b c d e)))

(print "BTREE-HEIGHT TESTS")
(print (BTREE-HEIGHT 1))
(print (BTREE-HEIGHT '(1 2)))
(print (BTREE-HEIGHT '(1 (2 3)))) 
(print (BTREE-HEIGHT '((1 2) (3 4)))) 
(print (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))) 
(print (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))))

(print "LIST2BTREE TESTS")
(print (LIST2BTREE '(1)))
(print (LIST2BTREE '(1 2)))
(print (LIST2BTREE '(1 2 3)))
(print (LIST2BTREE '(1 2 3 4)))
(print (LIST2BTREE '(1 2 3 4 5 6 7)))
(print (LIST2BTREE '(1 2 3 4 5 6 7 8)))

(print "BTREE2LIST TESTS")
(print (BTREE2LIST 1))
(print (BTREE2LIST '(1 2)))
(print (BTREE2LIST '((1 2) 3)))
(print (BTREE2LIST '((1 2) (3 4))))
(print (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))))
(print (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))))

(print "IS-SAME TESTS")
(print (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)))
(print (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)))
(print (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7)))