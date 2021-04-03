(load "hw1.lsp")

; TREE-CONTAINS TESTS
(assert (not (TREE-CONTAINS 3 NIL)))
(assert (TREE-CONTAINS 3 3))
(assert (TREE-CONTAINS 3 '(1 2 3)))
(assert (TREE-CONTAINS 3 '((1 2 3) 7 8)))
(assert (TREE-CONTAINS 3 '((1 2 3) 5 (6 8 (9 10 (11 12 13))))))
(assert (not (TREE-CONTAINS 5 3)))
(assert (not (TREE-CONTAINS 5 '(1 2 3))))
(assert (not (TREE-CONTAINS 5 '((1 2 3) 7 8))))
(assert (not (TREE-CONTAINS 333 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))))

; TREE-MIN TESTS 
(assert (= (TREE-MIN 3) 3))
(assert (= (TREE-MIN '((1 2 3) 7 8)) 1))
(assert (not (TREE-MIN NIL)))

; TREE-ORDER TESTS
(assert (not (TREE-ORDER NIL)))
(assert (IS-SAME (TREE-ORDER 3) '(3)))
(assert (IS-SAME (TREE-ORDER '((1 2 3) 7 8)) '(7 2 1 3 8)))
(assert (IS-SAME (TREE-ORDER '((1 2 3) 5 (6 8 (9 10 (11 12 13))))) '(5 2 1 3 8 6 10 9 12 11 13)))

; SUB-LIST TESTS
(assert (IS-SAME (SUB-LIST NIL 0 3) NIL))
(assert (IS-SAME (SUB-LIST '(1 2 3 4) 0 3) '(1 2 3)))
(assert (IS-SAME (SUB-LIST '(1 2 3 4) 1 2) '(2 3)))
(assert (IS-SAME (SUB-LIST '(1 2 3 4) 3 1) '(4)))
(assert (IS-SAME (SUB-LIST '(1 2 3 4) 2 0) NIL))

; SPLIT-LIST TESTS
(assert (IS-SAME (SPLIT-LIST NIL) NIL))
(assert (IS-SAME (SPLIT-LIST '(1)) '((1) NIL)))
(assert (IS-SAME (SPLIT-LIST '(1 2 3 4)) '((1 2) (3 4))))
(assert (IS-SAME (SPLIT-LIST '(1 2 3 4 5)) '((1 2 3) (4 5))))

; BTREE-HEIGHT TESTS
(assert (= (BTREE-HEIGHT 1) 0))
(assert (= (BTREE-HEIGHT '(1 2)) 1))
(assert (= (BTREE-HEIGHT '(1 (2 3))) 2))
(assert (= (BTREE-HEIGHT '((1 2) (3 4))) 2))
(assert (= (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3))
(assert (= (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3))

; LIST2BTREE TESTS
(assert (IS-SAME (LIST2BTREE '(1)) 1))
(assert (IS-SAME (LIST2BTREE '(1 2)) '(1 2)))
(assert (IS-SAME (LIST2BTREE '(1 2 3)) '((1 2) 3)))
(assert (IS-SAME (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4))))
(assert (IS-SAME (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7))))
(assert (IS-SAME (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8)))))

; BTREE2LIST TESTS
(assert (IS-SAME (BTREE2LIST 1) '(1)))
(assert (IS-SAME (BTREE2LIST '(1 2)) '(1 2)))
(assert (IS-SAME (BTREE2LIST '((1 2) 3)) '(1 2 3)))
(assert (IS-SAME (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4)))
(assert (IS-SAME (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7)))
(assert (IS-SAME (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8)))

; IS-SAME TESTS
(assert (IS-SAME 3 3))
(assert (not (IS-SAME 5 3)))
(assert (not (IS-SAME '(3) 3)))
(assert (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)))
(assert (not (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8))))
(assert (not (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7))))

(print "Passed all tests!")