; Q1: BFS
; ARGUMENTS: the list representation of a tree TREE
;
; RETURNS: a single, top-level list of the terminal nodes in TREE in the order 
; they would be visited by a left-to-right breadth-first search
;
; Treats the list representation as a queue by appending newly seen subtrees to
; the end for processing
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
;
; RETURNS: a single, top-level list of the terminal nodes in TREE in the order 
; they would be visited by a right-to-left depth-first search
;
; Treats the list representation as a stack by visiting newly seen subtrees 
; first, and also reverses the order of appending to the list to account for 
; right-to-left search
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
; ARGUMENTS: the list representation of a tree TREE, an integer representing 
; the max depth of the tree, M, and an integer representing the current depth
; of the search, D
;
; RETURNS: a single top-level list of the terminal nodes in TREE in the order 
; that they would be visited by a left-to-right depth-first search limited up
; to depth M
;
; Helper function that works very much like the DFS function but stops when it
; reaches depth M in its search
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
; ARGUMENTS: the list representation of a tree TREE, and an integer representing 
; the max depth of the tree, M
;
; RETURNS: a single top-level list of the terminal nodes in TREE in the order 
; that they would be visited by a left-to-right depth-first iterative-deepening 
; search
;
; Recursively calls and concats the output of DFID-SEARCH with maximum depth 
; values from 0 to M
(defun DFID (TREE M)
    (if (= M 0)
        NIL
        (append (DFID TREE (- M 1)) (DFID-SEARCH TREE 0 M))
    )
)

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
;
; Simply checks whether s is (3 3 NIL) using the equal function
(defun final-state (s)
    (equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
;
; Calculates next state based on s, m, and c and checks validity of this new state
(defun next-state (s m c)
    (let* ((m-curr (first s))
           (c-curr (second s))
           (m-other (- 3 m-curr))
           (c-other (- 3 c-curr)))
        (cond ((> m m-curr) NIL)
              ((> c c-curr) NIL)
            ((and (> (- m-curr m) 0) (< (- m-curr m) (- c-curr c))) NIL)
            ((and (> (+ m-other m) 0) (< (+ m-other m) (+ c-other c))) NIL)
            (t (list (list (+ m-other m) (+ c-other c) (not (third s))))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
;
; Uses next-state with possible values of missionaries and cannibals to move 
; to find valid successor states to the current state
(defun succ-fn (s)
    (append (next-state s 1 0)
            (next-state s 0 1)
            (next-state s 1 1)
            (next-state s 2 0)
            (next-state s 0 2)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
;
; Recursively checks the front of states to see if s is equal to some member of 
; states
(defun on-path (s states)
    (if (null states)
        NIL
        (let ((top-state (car states)))
            (if (equal s top-state)
                t
                (on-path s (cdr states))))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
;
; Uses mc-dfs to find a path from the head of states to the final state and
; returns a path if found; otherwise, recursively calls itself to check the
; other current states
(defun mult-dfs (states path)
    (if (null states) 
        NIL
        (let ((result (mc-dfs (car states) path)))
            (if (not (null result))
                result
                (mult-dfs (cdr states) path)
            )
        )
    )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
;
; Uses mult-dfs to find a path from s to the final state if s is not on the path
; and reverses its output into a left-to-right ordered path (i.e. as 
; (S_0 S_1 ... S_n))
(defun mc-dfs (s path)
    (cond ((final-state s) (append (list s) path))
          ((on-path s path) NIL)
          (t (reverse (mult-dfs (succ-fn s) (append (list s) path))))
    )
)

; TEST CASES
(print (equal (BFS '((A (B)) C (D))) '(C A D B)))
(print (equal (BFS '(A (B C) (D) (E (F G)))) '(A B C D E F G)))

(print (equal (DFS '((A (B)) C (D))) '(D C B A)))
(print (equal (DFS '(A (B C) (D) (E (F G)))) '(G F E D C B A)))
(print (equal (DFS '((A (B)) C ((D E) F G))) '(G F E D C B A)))

(print (equal (DFID '((A (B)) C (D)) 3) '(C A C D A B C D)))
(print (equal (DFID '(A (B C) (D) (E (F G))) 3) '(A A B C D E A B C D E F G)))

(print (not (next-state '(3 3 t) 1 0)))
(print (not (next-state '(1 1 t) 2 0)))
(print (equal (next-state '(3 3 t) 0 1) '((0 1 NIL))))

(print (equal (succ-fn '(3 3 t)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))
(print (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))

(print (equal (mc-dfs '(3 3 NIL) NIL) '((3 3 NIL))))
(print (mc-dfs '(3 3 t) NIL))