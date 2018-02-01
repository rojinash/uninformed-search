;;
;; File
;;   jump.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Provides procedures for generating momentum-based jumping problems
;;
;; Provides
;;   (jump-problem course-length)
;;   (jump-start-state course-length)
;;
;; Preconditions
;;   Assumes functions from problem.scm are defined
;;
;; Details
;;    A jump state is represented as a three element list. The first
;;    element in the list is an exact, non-negative integer
;;    representing the number of hops to the destination from the
;;    start. The second element is the "current" location of the
;;    agent, which is an exact, non-negative integer bounded above by
;;    the destination distance (inclusive). The third element is the
;;    magnitude of the previous jump. Note the third element is needed
;;    because the successor function only receives states, rather than
;;    search nodes (which would include the action).

;;
;; Procedure
;;   jump-problem
;;
;; Purpose
;;   Create a jump problem
;;
;; Parameters
;;   course-length, a positive integer
;;
;; Produces
;;   problem, a problem
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   jump-problem has a successor state function that only allows
;;   changes in hop distance according to the problem assignment (at
;;   most two more or one less than the previous hop
(define jump-problem
  (lambda (course-length)
    (make-problem
     ;; Goal-test function: 
     (lambda (state)
       (and (= (car state) (cadr state)); Location is last
	    (= (caddr state) 1)))       ; Reached by single hop
     ;; Successor function
     (lambda (state)
       (let ((actions (jump-actions state)))
	 (map (lambda (action)
		(cons action (jump-action-result state action)))
	      actions)))
     ;; Stepcost function
     (lambda (state action)
       1)))) ; unit stepcost

;;
;; Procedure
;;   jump-start-state
;;
;; Purpose
;;   Create the jump problem start state
;;
;; Parameters
;;   course-length, a positive integer
;;
;; Produces
;;   start-state, a problem state
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   jump-start-state is the three element list as described in this file's header:
;;     (course-length 0 0)
(define jump-start-state
  (lambda (course-length)
    (list course-length 0 0)))

;;
;; Procedure
;;   jump-actions
;;
;; Purpose
;;   Create the list of available actions from the current state
;;
;; Parameters
;;   state, a jump problem state
;;
;; Produces
;;   actions, a list
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   actions is a list of exact integers corresponding to the possible jump lengths
(define jump-actions
  (lambda (state)
    (let* ((size (car state))          ; course size
	   (current (cadr state))      ; current location
	   (prev (abs (caddr state)))  ; previous action magnitude
	   (to-go (- size current)))    ; number of rightward hops left
      (cond
       ((zero? current) ; Start state?
	(list 1)) ; Must jump only one hop
       ((> current size) ; Jumped too far?
	null) ; no actions
       (else
	(let ((max-delta 2)
	      (min-delta -1))
	  (let loop ((actions null)
		     (move-dir 1) ; Change to -1 to allow negative movements
		     (delta min-delta))
	    (if (> delta max-delta) ; Ran out of leftward moves
		(if (negative? move-dir)
		    (loop actions 1 min-delta) ; Start over with rightward moves
		    actions)                    ; All done.
		(let* ((action (* move-dir (+ prev delta)))
		       (result (+ current action)))
		  (if (or (zero? action)     ; useless action or out of bounds result?
			  (negative? result)
			  (> result size))
		      (loop actions move-dir (+ 1 delta)) ; Skip action
		      (loop (cons action actions) ; Add move to actions
			    move-dir
			    (+ 1 delta))))))))))))
  
;;
;; Procedure
;;   jump-action-result
;;
;; Purpose
;;   Create the state resulting from taking an action in the current state
;;
;; Parameters
;;   state, a jump problem state
;;   action, an integer
;;
;; Produces
;;   new-state, a list
;;
;; Preconditions
;;   According to the game definition,  
;;     prev-1 <= action <= prev+2
;;   where prev is (caddr state)
;;
;; Postconditions
;;   new-state is a the three-element list containing the course size,
;;   the new loation, and the specified action
(define jump-action-result
  (lambda (state action)
    (list (car state)             ; course size
	  (+ (cadr state) action) ; new location
	  action)))               ; "previous" hop