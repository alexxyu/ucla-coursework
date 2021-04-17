;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;
; check-row (r)
; recursively checks whether the current square within row r is a box or a keeper
;
(defun check-row (r)
  (cond ((null r) t)
		((or (isBox (car r)) (isKeeper (car r))) nil)
		(t (check-row (cdr r)))
  )
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; goal-test (s)
; checks every row in s to see whether there are any boxes or keeper that is on a non-goal square
;
(defun goal-test (s)
  (cond ((null s) t)
		((check-row (car s)) (goal-test (cdr s)))
		(t nil)
  )
  );end defun

; 
; get-row (s y)
; recursively finds and returns the row with index y within the state s
;
(defun get-row (s y)
  (if (= y 0)
    (car s)
	(get-row (cdr s) (- y 1))
  )
)

;
; get-col (r x) 
; recursively finds and returns the square with index x within the row r
;
(defun get-col (r x)
  (if (= x 0)
	(car r)
	(get-col (cdr r) (- x 1))
  )
)

;
; get-square (s x y)
; uses get-row and get-col to find and return the value of the square at row y 
; column x within state s
; 
(defun get-square (s x y)
  (let ((square (get-col (get-row s y) x)))
	(if (null square)
	  wall
	  square
	)
  )
)

;
; set-col (r x v acc)
; recursively constructs a new row to be a copy of the given row r, with the
; square at index x within the new row to be value v
;
(defun set-col (r x v acc)
  (if (= x 0)
    (append acc (list v) (cdr r))
	(set-col (cdr r) (- x 1) v (append acc (list (car r))))
  )
)

;
; set-row (s x y v acc)
; recursively constructs a new state to be a copy of the given state s, with
; the row at index y within the new state to have its square at index x set to 
; the value v
;
(defun set-row (s x y v acc)
  (if (= y 0)
	(append acc (list (set-col (car s) x v nil)) (cdr s))
	(set-row (cdr s) x (- y 1) v (append acc (list (car s))))
  )
)

;
; set-square (s x y v)
; returns the state s with the square at column y row x set to the value v, 
; using set-row as its helper function
;
(defun set-square (s x y v)
  (set-row s x y v nil)
)

;
; get-pos-in-dir (x y d)
; returns the new position if one moves 1 unit in the direction d ('U, 'D, 'L,
; or 'R) from position (x, y)
;
(defun get-pos-in-dir (x y d)
  (cond ((equal d 'U) (list x (- y 1)))
		((equal d 'D) (list x (+ y 1)))
		((equal d 'L) (list (- x 1) y))
		(t (list (+ x 1) y))
  )
)

;
; try-move-box (s x y d)
; returns the new state if one can move the box at position (x, y) in the given
; state s by checking whether the square in the given direction d is a blank or 
; a goal; otherwise, it returns nil
;
(defun try-move-box (s x y d)
  (let* ((new-pos (get-pos-in-dir x y d))
	 (new-x (car new-pos))
	 (new-y (cadr new-pos))
	 (curr-square (get-square s x y))
	 (next-square (get-square s new-x new-y))
  	 )
	(cond ((isBlank next-square) 
			(set-square (set-square s new-x new-y box) x y (get-sole-square-value curr-square))
		  )
		  ((isStar next-square) 
		  	(set-square (set-square s new-x new-y boxstar) x y (get-sole-square-value curr-square))
		  )
		  (t nil)
	)   
  )
)

;
; get-sole-square-value (v)
; returns the value of a square if a keeper or box was moved out of it (e.g.,
; if the keeper moves out of a keeper-goal square, that square becomes a goal
; square)
;
(defun get-sole-square-value (v)
  (cond ((isBoxStar v) star)
		((isKeeperStar v) star)
		(t blank)
  )
)

;
; try-move (s x y d)
; returns the new state if one can move the keeper at position (x, y) in
; direction d given current state s by checking whether the new square
; is blank or a goal. If a box is going to be pushed, it will also 
; check whether that is a valid move using try-move-box; otherwise, 
; it returns nil
;
(defun try-move (s x y d)
  (let* ((new-pos (get-pos-in-dir x y d))
	 (new-x (car new-pos))
	 (new-y (cadr new-pos))
	 (curr-square (get-square s x y))
	 (next-square (get-square s new-x new-y))
  	 )
	(cond ((isWall next-square) nil)
		  ((isBlank next-square) 
		  	(set-square (set-square s new-x new-y keeper) x y (get-sole-square-value curr-square))
		  )
	      ((isStar next-square) 
		  	(set-square (set-square s new-x new-y keeperstar) x y (get-sole-square-value curr-square))
		  )
		  (t 
		  	(let ((move-box-result (try-move-box s new-x new-y d)))
			  (if (null move-box-result) 
			  	nil
				(try-move move-box-result x y d)
			  )
			)
		  )
	)
  )
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
; next-states (s)
; returns the list of successor states of s using try-move as a helper function
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s x y 'U) (try-move s x y 'D) (try-move s x y 'L) (try-move s x y 'R)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
; h0 (s)
; simply returns 0
;
(defun h0 (s)
  0
  )

;
; count-misplaced-in-row (r count)
; recursively counts the number of boxes in row r (count is the accumulator)
;
(defun count-misplaced-in-row (r count)
  (cond ((null r) count)
		((isBox (car r)) (count-misplaced-in-row (cdr r) (+ count 1)))
		(t (count-misplaced-in-row (cdr r) count))
    )
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; h1 (s)
; returns the number of misplaced boxes in state s using count-misplaced-in-row. 
; This is an admissable heuristic because each misplaced box requires at least 
; one move for it to reach a goal position
;
(defun h1 (s)
  (cond ((null s) 0)
		(t (+ (count-misplaced-in-row (car s) 0) (h1 (cdr s))))
    )
  )

;
; get-boxes-in-row (r x y acc)
; recursively adds (x, y) to the accumulator acc if the square at row y column 
; x in state s is a box or is the keeper
;
(defun get-boxes-in-row (r x y acc)
  (cond ((null r) acc)
		((or (isKeeper (car r)) (isBox (car r))) (get-boxes-in-row (cdr r) (+ x 1) y (cons (list x y) acc)))
		(t (get-boxes-in-row (cdr r) (+ x 1) y acc))
  )
)

;
; get-boxes-in-rows (s y acc)
; recursively checks all rows for boxes and the keeper in state s and adds 
; their positions to the accumulator acc
;
(defun get-boxes-in-rows (s y acc)
  (cond ((null s) acc)
		(t (get-boxes-in-rows (cdr s) (+ y 1) (append acc (get-boxes-in-row (car s) 0 y nil))))
  )
)

;
; get-boxes (s)
; returns a list of all positions of the boxes and the keeper in the current
; state s using get-boxes-in-rows as a helper function
;
(defun get-boxes (s)
  (get-boxes-in-rows s 0 nil)
)

;
; get-goals-in-row (r x y acc)
; recursively adds (x, y) to the accumulator acc if the square at row y column 
; x is a goal
;
(defun get-goals-in-row (r x y acc)
  (cond ((null r) acc)
		((isStar (car r)) (get-goals-in-row (cdr r) (+ x 1) y (cons (list x y) acc)))
		(t (get-goals-in-row (cdr r) (+ x 1) y acc))
  )
)

;
; get-goals-in-rows (s y acc)
; recursively checks all rows for goals in state s and adds their positions
; to the accumulator acc
;
(defun get-goals-in-rows (s y acc)
  (cond ((null s) acc)
		(t (get-goals-in-rows (cdr s) (+ y 1) (append acc (get-goals-in-row (car s) 0 y nil))))
  )
)

;
; get-boxes (s)
; returns a list of all positions of the goals in the current state s using 
; get-goals-in-rows as a helper function
;
(defun get-goals (s)
  (get-goals-in-rows s 0 nil)
)

;
; get-dist (c1 c2)
; returns the Manhattan distance between two positions c1 and c2
;
(defun get-dist (c1 c2)
  (+ (abs (- (car c1) (car c2))) (abs (- (cadr c1) (cadr c2))))
)

;
; get-min-dist-to-goal (box goals acc)
; recursively finds the minimum distance between the given box's position and 
; any one position of the goals (acc is the accumulator of the minimum value)
;
(defun get-min-dist-to-goal (box goals acc)
  (if (null goals)
    acc
  	(let ((goal (car goals)))
      (get-min-dist-to-goal box (cdr goals) (min (get-dist box goal) acc))
	)
  )
)

;
; sum-box-dists-to-goals (boxes goals acc)
; recursively calculates the sum of distances between each box and the closest
; goal square, using get-min-dist-to-goal as a helper function
;
(defun sum-box-dists-to-goals (boxes goals acc)
  (if (null boxes)
    acc
	(sum-box-dists-to-goals (cdr boxes) goals (+ acc (get-min-dist-to-goal (car boxes) goals 100)))
  )
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; h105295708 (s)
; returns the sum of the Manhattan distance between each box and the closest 
; goal, given state s, using sum-box-dists-to-goals as a helper function
;
(defun h105295708 (s)
  (let ((boxes (get-boxes s))
	 (goals (get-goals s))
	 )
	(sum-box-dists-to-goals boxes goals 0)
  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
