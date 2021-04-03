; Q1: TREE-CONTAINS takes a number N and an ordered tree TREE, and returns whether N appears in TREE
; Checks whether middle number is N, otherwise recursively checks left and right subtrees
(defun TREE-CONTAINS (N TREE)
    (if (null TREE)
        NIL
        (if (listp TREE)
            (if (> (second TREE) N)
                (TREE-CONTAINS N (first TREE))
                (TREE-CONTAINS N (third TREE))
            )
            (= N TREE)
        )
    )
)

; Q2: TREE-MIN takes an ordered tree TREE and returns the minimum number in TREE
; Recursively finds the bottom-leftmost number in TREE since that is the minimum number
(defun TREE-MIN (TREE)
    (if (null TREE)
        NIL
        (if (not (listp TREE))
            TREE
            (TREE-MIN (first TREE))
        )
    )
)

; Q3: TREE-ORDER takes an ordered tree TREE and returns a pre-ordered list of numbers appearing in TREE
; Creates a list out of the middle number and the recusively created lists of the left subtree and right subtree
(defun TREE-ORDER (TREE)
    (if (not (null TREE))
        (if (not (listp TREE)) 
            (list TREE)
            (append (cons (second TREE) (TREE-ORDER (first TREE))) (TREE-ORDER (third TREE)))
        )
    )
)

; Q4: SUB-LIST takes a list L and two non-negative integers START and LEN, and returns a sub-list of L starting 
;     from position START with length LEN
; Recursively moves to position START in L and then recursively adds LEN number of elements in L to the sub-list
(defun SUB-LIST (L START LEN)
    (if (null L)
        NIL
        (if (> START 0)
            (SUB-LIST (rest L) (- START 1) LEN)
            (if (> LEN 0)
                (cons (car L) (SUB-LIST (rest L) START (- LEN 1)))
                NIL
            )
        )
    )
)

; Q5: SPLIT-LIST takes a list L and returns two lists, L1 and L2, such that L is the result of appending L1 and L2,
;     and the length of L1 minus the length of L2 is either 0 or 1
; Uses the SUB-LIST function to generate the two lists, using the length of L to calculate START and LEN for L1 and L2
(defun SPLIT-LIST (L)
    (let ((x (ceiling (/ (length L) 2))))
        (if (oddp (length L))
            (list (SUB-LIST L 0 x) (SUB-LIST L x (- x 1)))
            (list (SUB-LIST L 0 x) (SUB-LIST L x x))
        )
    )
)

; Q6: BTREE-HEIGHT takes a binary tree TREE and returns the height of TREE
; Recusively counts the deepest level of a nested list within TREE
(defun BTREE-HEIGHT (TREE)
    (if (or (atom TREE) (null TREE))
        0
        (+ 1 (max (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (cadr TREE))))
    )
)

; Q7: LIST2BTREE takes a list of atoms LEAVES and returns a binary tree such that the tree leaves are elements of
;     LEAVES, and for any internal node, the number of leaves in its left branch minus the number of leaves in its
;     right branch is either 0 or 1
; Recursively divides LEAVES using SPLIT-LIST and puts the generated sub-lists into a list
(defun LIST2BTREE (LEAVES)
    (if (<= (length LEAVES) 1)
        (car LEAVES)
        (let ((split (SPLIT-LIST LEAVES)))
            (list (LIST2BTREE (first split)) (LIST2BTREE (second split)))
        )
    )
)

; Q8: BTREE2LIST takes a binary tree TREE and returns a flattened list of the leaves in TREE
; Recursively unpacks each nested list into a flattened list and appends the resulting lists together
(defun BTREE2LIST (TREE)
    (if (not (listp TREE))
        (list TREE)
        (if (null TREE)
            NIL
            (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE)))
        )
    )
)

; Q9: IS-SAME takes LISP expressions E1 and E2 (whose atoms are all numerical) and checks whether they are identical
; Recursively checks each corresponding list and atom within E1 and E2 and checks equality using '='
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