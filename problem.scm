;;
;; File
;;   problem.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Provides a collection of routines for defining a search space problem
;;
;; Provides
;;   (make-problem goal? successor-fun stepcost-fun)
;;   (problem-goal? problem)
;;   (problem-successor-fun problem)
;;   (problem-stepcost-fun problem)
;;   (problem-expand-node problem node heuristic)

;;
;; Procedure
;;   make-problem
;;
;; Purpose
;;   Create a problem
;;
;; Parameters
;;   goal?, a procedure
;;   successor-fun, a procedure
;;   stepcost-fun, a procedure
;;
;; Produces
;;   problem, a problem
;;
;; Preconditions
;;   goal? is a predicate procedure that takes a state for the problem
;;   successor-fun is a procedure that takes a state for the problem an 
;;      returns a list of pairs, each of whose car is an action and whose 
;;      cdr is the resulting state
;;   stepcost-fun is a procedure that takes a state and an action, 
;;      producing a number
;;
;; Postconditions
;;   [None]
(define make-problem
  (lambda (goal? successor-fun stepcost-fun)
    (list goal? successor-fun stepcost-fun)))


;;
;; Procedure
;;   problem-goal?
;;
;; Purpose
;;   Return a predicate for testing whether a state is a solution
;;
;; Parameters
;;   problem, a problem
;;
;; Produces
;;   goal?, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define problem-goal? car)

;;
;; Procedure
;;   problem-successor-fun
;;
;; Purpose
;;   Return a function that generates successors for a problem state
;;
;; Parameters
;;   problem, a problem
;;
;; Produces
;;   successor-fun, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   successor-fun is a procedure that takes a state for the problem an 
;;      returns a list of pairs, each of whose car is an action and whose 
;;      cdr is the resulting state
(define problem-successor-fun cadr)

;;
;; Procedure
;;   problem-stepcost-fun
;;
;; Purpose
;;   Retrieve a function for calculating a problem's stepcost
;;
;; Parameters
;;   problem, a problem
;;
;; Produces
;;   stepcost-fun, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   stepcost-fun takes a state and an action and produces a number
(define problem-stepcost-fun caddr)




;;
;; Procedure
;;   problem-expand-node
;;
;; Purpose
;;   Produce a list of nodes that may follow actions from a given node
;;
;; Parameters
;;   problem, a problem
;;   node, a node
;;   heuristic, a procedure
;;
;; Produces
;;   successors, a list
;;
;; Preconditions
;;   node contains a state that is valid for the problem
;;   heuristic takes a state and produces a number
;;
;; Postconditions
;;   successors contains a list of nodes 
(define problem-expand-node
  (lambda (problem node heuristic)
    (let loop ((successors ((problem-successor-fun problem) (node-state node))))
      (if (null? successors)
          null ;; No more next-states
          (if (node-search-repeat? node (cdar successors))
              (loop (cdr successors))
              (let* ((path-cost (+ (node-path-cost node) 
                                   ((problem-stepcost-fun problem)
                                    (node-state node) (caar successors))))
                     (total-cost (+ path-cost (heuristic (cdar successors)))))
                (cons 
                 (make-node (caar successors) ; action
                            (cdar successors) ; state
                            node              ; parent-node
                            path-cost
                            total-cost
                            (+ 1 (node-depth node))) ;depth
                 (loop (cdr successors)))))))))
  


