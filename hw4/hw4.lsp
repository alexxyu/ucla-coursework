;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
	(reverse (search-var (get-vars n nil) delta nil)))

; get-vars returns a list of numbers from 1 to n
(defun get-vars (n accum)
  (if (= n 0)
    accum
    (get-vars (- n 1) (cons n accum))
    )
  )

; search-var recursively assigns a variable a true value, adds it to the model, and 
; continues searching if valid. It then does the same, only assigning the same 
; variable a false value. If it finds a valid model with all n variables, it will 
; return that
(defun search-var (vars delta model)
  (if (null vars)
    model
    (let* ((var (car vars))
          (m1 (cons var model))
          (m2 (cons (- var) model)))
      (if (validate-to-delta delta m1)
        (let ((m1-result (search-var (cdr vars) delta m1)))
          (if (not (null m1-result))
            m1-result
            (if (validate-to-delta delta m2)
              (search-var (cdr vars) delta m2)
              nil
              )
            )
          )
        (if (validate-to-delta delta m2)
          (search-var (cdr vars) delta m2)
          nil
          )
        )
      )
    )
  )

; validate-to-delta checks whether the model satisfies the given delta
(defun validate-to-delta (delta model)
  (cond ((null delta) t)
        ((not (validate-to-clause (car delta) model)) nil)
        (t (validate-to-delta (cdr delta) model))
    )
  )

; validate-to-clause checks whether the model satisifes the given clause
(defun validate-to-clause (clause model) 
  (cond ((null clause) nil)
        ((member (car clause) model) t)
        ((and (not (member (car clause) model)) (not (member (- (car clause)) model))) t)
        (t (validate-to-clause (cdr clause) model))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

