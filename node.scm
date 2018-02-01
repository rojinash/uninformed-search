;;
;; File
;;   node.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Provides a collection of routines for nodes in a search space problem
;;
;; Provides
;;   (make-node action state parent-node path-cost total-cost depth)
;;   (node-action node)
;;   (node-state node)
;;   (node-parent node)
;;   (node-path-cost node)
;;   (node-total-cost node)
;;   (node-depth node)
;;   (node-init state heuristic)
;;   (node-search-repeat? node state)
;;   (node-extract-actions node)


;;
;; Procedure
;;   make-node
;;
;; Purpose
;;   Create a node
;;
;; Parameters
;;   action, a value
;;   state, a value
;;   parent-node, a node
;;   path-cost, a number
;;   total-cost, a number
;;   depth, a number
;;
;; Produces
;;   node, a node
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define make-node
  (lambda (action state parent-node path-cost total-cost depth)
    (list action state parent-node path-cost total-cost depth)))


;;
;; Procedure
;;   node-action
;;
;; Purpose
;;   Get the action for a node
;;
;; Parameters
;;   node, a node
;;
;; Produces
;;   action, a value
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define node-action car)

;;
;; Procedure
;;   node-state
;;
;; Purpose
;;   Get the state for a node
;;
;; Parameters
;;   node, a node
;;
;; Produces
;;   state, a value
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define node-state cadr)

;;
;; Procedure
;;   node-parent
;;
;; Purpose
;;   Get the parent node for a node
;;
;; Parameters
;;   node, a node
;;
;; Produces
;;   parent, a node
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define node-parent caddr)

;;
;; Procedure
;;   node-path-cost
;;
;; Purpose
;;   Get the path-cost for a node
;;
;; Parameters
;;   node, a node
;;
;; Produces
;;   path-cost, a number
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define node-path-cost cadddr)

;;
;; Procedure
;;   node-total-cost
;;
;; Purpose
;;   Get the (estimated) total-cost for a node
;;
;; Parameters
;;   node, a node
;;
;; Produces
;;   total-cost, a number
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   [None]
(define node-total-cost
  (lambda (node) (list-ref node 4)))

;;
;; Procedure
;;   node-depth
;;
;; Purpose
;;   Get the depth of a node
;;
;; Parameters
;;   node, a node
;;
;; Produces
;;   depth, a number
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   depth >= 0
(define node-depth 
  (lambda (node) (list-ref node 5)))

;;
;; Procedure
;;   node-init
;;
;; Purpose
;;   Create an initial node from a start state
;;
;; Parameters
;;   state, a state
;;   heuristic, a procedure
;;
;; Produces
;;   node, a node
;;
;; Preconditions
;;   heuristic takes a state and produces a number
;;
;; Postconditions
;;   [None]
(define node-init
  (lambda (state heuristic)
    (make-node
     '()
     state
     '()
     0
     (heuristic state)
     0)))

;;
;; Procedure
;;   node-search-repeat?
;;
;; Purpose
;;   Determine whether a state has been visited along a node's search path
;;
;; Parameters
;;   node, a node
;;   state, a value
;;
;; Produces
;;   repeat?, a boolean
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   repeat? is true if there exists an ancestor of node with the given state
(define node-search-repeat?
  (lambda (node state)
    (let loop ((path-node node))
      (cond 
       ((null? path-node) ;; No more nodes to check
        #f)
       ((equal? state (node-state path-node)) ;; a match
        #t)
       (else
        (loop (node-parent path-node))))))) ;; any other matches


;;
;; Procedure
;;   node-extract-actions
;;
;; Purpose
;;   Get the list of actions on the path from this node to the search tree root
;;
;; Parameters
;;   node, a node
;;
;; Produces
;;   actions, a list
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   If node is null, actions is null.
;;   Otherwise, the first element in actions is the first (oldest)
;;   action taken, while the last element in actions is (node-action
;;   node).
(define node-extract-actions 
  (lambda (node)
    (let loop ((node node)
	       (actions '()))
      (if (null? (node-parent node))
	  actions
	  (loop (node-parent node)
		(cons (node-action node)
		      actions))))))

