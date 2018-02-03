;;
;; File
;;   driver.scm
;;
;; Authors
;;   Nannan Ji
;;   Rojina Sharma
;;    
;; Summary
;;   1) 3Ps for testing procedures
;;   2) 12 test cases using jump probelem and 8puzzle problem
;;
;; Provides
;;   (fix-DLS-limit limit)
;;   (run-test-jump search course-length)
;;   (run-test-8puzzle search)
;;   (display-result sol)
;;   (run-all-tests)

(load "node.scm")
(load "problem.scm")
(load "sort.scm")
(load "jump.scm")
(load "search.scm")
(load "8puzzle.scm")

;;
;; Procedure
;;   fix-DLS-limit
;;
;; Purpose
;;   fix the limit parameter of a DLS algorithm
;;
;; Preconditions
;;   limit >= 0
;;
;; Postconditions
;;   result is a procedure that takes two parameters
;;   "start" and "problem," with the 3rd parameter fixed
;;   to limit
(define fix-DLS-limit
  (lambda (limit)
    (lambda (start problem)
      (depth-limited-search start problem limit))))

;;
;; Procedure
;;   run-test-jump
;;
;; Purpose
;;   perform the search on a jump problem 
;;
;; Preconditions
;;   course-length>=0
;;   search takes a start state and a problem and produce a solution
;;
;; Postcondition
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state or #f if no solution could be found, and
;;   num-expansions is a number indicating the number of times
;;   problem-expand-node is called. 
(define run-test-jump
  (lambda (search course-length)
    (let ([start (jump-start-state course-length)]
          [problem (jump-problem course-length)])
      (search start problem))))

;;
;; Procedure
;;   run-test-8puzzle
;;
;; Purpose
;;   perform the search on a 8puzzle problem 
;;
;; Preconditions
;;   search takes a start state and a problem and produce a solution
;;
;; Postcondition
;;   result is a list of the form (solution num-expansions), where
;;   solution is a list of actions that can be taken to reach a goal
;;   state from start-state or #f if no solution could be found, and
;;   num-expansions is a number indicating the number of times
;;   problem-expand-node is called. 
(random-seed 60)
(define run-test-8puzzle
  (lambda (search)
    (let ([start (random-eight-puzzle-state 1)])
      (search start (eight-puzzle-problem)))))


;;
;; Procedure
;;   display-result
;;
;; Purpose
;;   display the result of a sol found a certain search algorithm
;;
;; Preconditions
;;   sol is a list of the form (solution num-expansions)
;;
;; Postcondition
;;   [No Additional] 
(define display-result
  (lambda (sol)
    (if (list? (car sol))
        (begin
          (display sol)
          (display "Length of path:")
          (display (length (car sol)))
          (newline)
          (display "Expansions:")
          (display (cadr sol))
          (newline))
        (begin
          (display "No solution found.")
          (newline)))))

;;
;; Procedure
;;   display-result
;;
;; Purpose
;;   run all tests with differen search algorithms and problems 
;;
;; Preconditions
;;   [No Additional]
;;
;; Postcondition
;;   [No Additional]
(define run-all-tests
  (lambda ()
    ;;------------
    ;;   Test 1  |
    ;;------------
    ;; State initialization:
    ;;
    ;; Probelm: Jump
    ;;
    ;; Course-length: 3
    (define course-length-1 3)
    (display "Running test1 for jump using DFS")
    (newline)
    (display-result (run-test-jump depth-first-search course-length-1))
    (display "------------------------------------")
    (newline)

    (display "Running test1 for jump using BFS")
    (newline)
    (display-result (run-test-jump breadth-first-search course-length-1))
    (display "------------------------------------")
    (newline)

    (display "Running test1 for jump using DLS")
    (newline)
    (display-result (run-test-jump (fix-DLS-limit 5) course-length-1))
    (display "------------------------------------")
    (newline)

    (display "Running test1 for jump using IDS")
    (newline)
    (display-result (run-test-jump iterative-deepening-search course-length-1))
    (display "------------------------------------")
    (newline)

    ;;------------
    ;;   Test 2  |
    ;;------------
    ;; State initialization:
    ;;
    ;; Probelm: Jump
    ;;
    ;; Course-length: 5
    (define course-length-5 5)
    (display "Running test2 for jump using DFS")
    (newline)
    (display-result (run-test-jump depth-first-search course-length-5))
    (display "------------------------------------")
    (newline)

    (display "Running test2 for jump using BFS")
    (newline)
    (display-result (run-test-jump breadth-first-search course-length-5))
    (display "------------------------------------")
    (newline)

    (display "Running test2 for jump using DLS")
    (newline)
    (display-result (run-test-jump (fix-DLS-limit 5) course-length-5))
    (display "------------------------------------")
    (newline)

    (display "Running test2 for jump using IDS")
    (newline)
    (display-result (run-test-jump iterative-deepening-search course-length-5))
    (display "------------------------------------")
    (newline)

    ;;------------
    ;;   Test 3  |
    ;;------------
    ;; State initialization:
    ;;
    ;; Probelm: 8puzzle
    ;;
    (display "Running test3 for 8puzzle using DFS")
    (newline)
    (display-result (run-test-8puzzle depth-first-search))
    (display "------------------------------------")
    (newline)

    (display "Running test3 for 8puzzle using BFS")
    (newline)
    (display-result (run-test-8puzzle breadth-first-search))
    (display "------------------------------------")
    (newline)

    (display "Running test3 for 8puzzle using DLS")
    (newline)
    (display-result (run-test-8puzzle (fix-DLS-limit 3) ))
    (display "------------------------------------")
    (newline)

    (display "Running test3 for 8puzzle using IDS")
    (newline)
    (display-result (run-test-8puzzle iterative-deepening-search))
    (display "------------------------------------")
    (newline)   
    ))

(run-all-tests)





