;;
;; File
;;   hexturn.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Provides procedures for generating hex-turn puzzle problems
;;
;; Provides
;;   (hexturn-problem num-rings)
;;   (random-hexturn-puzzle num-rings)
;;   (hexturn-moves state)
;;   (hexturn-board state)
;;   (hexturn-goal state)
;;   (hexturn-num-rings state)
;;   (hexturn-color state)
;;   (hexturn-board-color board tile)
;;   (hexturn-board-print num-rings board)
;;
;; Preconditions
;;   Assumes functions from problem.scm are defined
;;
;; Props
;;   This puzzle was inspired by a similar puzzle from Erich Friedman:
;;     http://www2.stetson.edu/~efriedma/hexturn/
;;
;; Particulars
;;    A hexturn state is represented as a list.
;;      * a list of pairs representing the tiles visited along the solution
;;        path so far, in reverse order (i.e., current is first, start is last)
;;      * packed representation of the board's colorings
;;      * a pair of integers representing the goal tile location (start tile is
;;        always the center)
;;      * non-negative integer representing the number of rings in the hex
;;        puzzle
;;   While only the previous two tiles are needed to determine the curvature of
;;   a move, tiles must be visited only once, thus all tiles are needed to
;;   prevent repeat visits.
;;
;;   The start state is always (0 . 0).
;;
;;   Note that to conserve memory, all states share all pairs and vectors in
;;   the structure except the one containing the pair/tile with the most recent
;;   move (a flyweight pattern).

(load "problem.scm")

(define hexturn-problem
  (lambda (num-rings)
    (make-problem
     ;; Goal-test function
     hexturn-goal?
     ;; Successor function
     (lambda (state)
       (let ([actions (hexturn-actions state)])
         (map (lambda (action)
                (cons action (hexturn-action-result state action)))
              actions)))
     ;; Stepcost function
     (lambda (state action) 1) ; unit stepcost
     )))

(define hexturn-moves car)
(define hexturn-board cadr)
(define hexturn-goal caddr)
(define hexturn-num-rings cadddr)

;;
;; Procedure
;;   hexturn-actions
;;
;; Purpose
;;   Generate list of available hexturn moves from the given state
;;
;; Parameters
;;   state, a hexturn puzzle state
;;
;; Produces
;;   actions, a list of pairs of integers
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   actions contains the list of neighboring hex tiles, subject to the two
;;    problem constraints: no repeats and color-allowable curvature
;;   actions is a subset of
;;     (hexturn-tile-neighbors (car (hexturn-moves state)))
;;
(define hexturn-actions
  (lambda (state)
    (let* ([move-history (hexturn-moves state)]
           [current-tile (car move-history)]
           [neighbors
            (hexturn-tile-neighbors (hexturn-num-rings state) current-tile)]
           [color (hexturn-color state)]
           ; Predicate to determine valid moves of neighboring tiles
           [valid-move? (lambda (tile)
                          (and
                           (not (member tile move-history)) ; Unvisited neighbor
                           (or (equal? color 'white) ; White (no move constraints)
                               (and                  ; Yellow and gentle or
                                (equal? color 'yellow) ;  straight (not sharp)
                                (not
                                 (equal?
                                  'sharp
                                  (hexturn-move-direction tile
                                                          current-tile
                                                          (cadr move-history)))))
                               (and                    ; Orange and sharp or
                                (equal? color 'orange) ; straight (not gentle)
                                (not
                                 (equal?
                                  'gentle
                                  (hexturn-move-direction tile
                                                          current-tile
                                                          (cadr move-history))))))))])
      (filter valid-move? neighbors))))


;;
;; Procedure
;;   hexturn-action-result
;;
;; Purpose
;;   Generate state resulting from an action (transition function)
;;
;; Parameters
;;   state, a hexturn puzzle state
;;   action, a pair of integers
;;
;; Produces
;;   next-state, a list
;;
;; Preconditions
;;   action is a valid action for state, that is:
;;     (member action (hexturn-actions state))
;;
;; Postconditions
;;   adds action to list of moves, preserving all other state elements (which
;;   saves memory via the flyweight pattern)
;;
(define hexturn-action-result
  (lambda (state action)
    (cons (cons action (hexturn-moves state))
          (cdr state))))

;;
;; Procedure
;;   hexturn-goal?
;;
;; Purpose
;;   Determines whether a hexturn state has reached the goal
;;
;; Parameters
;;   state, a hexturn puzzle state
;;
;; Produces
;;   goal?, a boolean
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   goal? is #t when the most recent move has led to the goal tile
;;   goal? should produce #f for any other game state
(define hexturn-goal?
  (lambda (state)
    (let* ([move-history (hexturn-moves state)]
           [current-tile (car move-history)])
      (equal? current-tile
              (hexturn-goal state)))))



;;
;; Procedure
;;   hexturn-num-tiles
;;
;; Purpose
;;   Calculate the number of hex tiles in a hexturn puzzle
;;
;; Parameters
;;   num-rings, an integer
;;
;; Produces
;;   num-tiles, an integer
;;
;; Preconditions
;;   num-rings > 0
;;
;; Postconditions
;;   num-tiles = 1 + (* 6 (sum (iota num-rings)))
(define hexturn-num-tiles
  (lambda (num-rings)
    (+ 1 (* 3 num-rings (- num-rings 1)))))

;;
;; Procedure
;;   hexturn-tile-neighbors
;;
;; Purpose
;;   Generate the list of tile neighbors
;;
;; Parameters
;;   num-rings, an integer
;;   tile, a pair of integers
;;
;; Produces
;;   neighbors, a list of pairs of integers
;;
;; Preconditions
;;   num-rings > 0
;;   tile represents a valid tile address, (i . j)
;;
;; Postconditions
;;   neighbors contains the six neighboring elements:
;;     (i  , j+2)  [east]
;;     (i  , j-2)  [west]
;;     (i+1, j-1)  [southwest]
;;     (i-1, j-1)  [northwest]
;;     (i+1, j+1)  [southeast]
;;     (i-1, j+1)  [northwest]
;;   subject to the limits of the hex puzzle dimensions
(define hexturn-tile-neighbors
  (lambda (num-rings tile)
    (let* ([i (car tile)] ; First, construct the list of all possible neighbors
           [j (cdr tile)]
           [all-neighbors (list (cons i (+ j 2))         ; east
                                (cons i (- j 2))         ; west
                                (cons (+ i 1) (- j 1))   ; southwest
                                (cons (- i 1) (- j 1))   ; northwest
                                (cons (+ i 1) (+ j 1))   ; southeast
                                (cons (- i 1) (+ j 1)))]); northwest
      (let* ([max-row (- num-rings 1)] ; Then, calculate the bounds
             [min-row (- max-row)]
             [max-col (lambda (i) (- (* 2 (- num-rings 1))
                                     (abs i)))]
             [min-col (lambda (i) (- (max-col i)))]
             [valid-tile? (lambda (tile) ; Whether neighbor is in bounds
                            (let ([in (car tile)]
                                  [jn (cdr tile)])
                              (and (<= min-row in max-row)
                                   (<= (min-col in) jn (max-col in)))))])
        (filter valid-tile? all-neighbors))))) ; Filter, preserving valid tiles


;;
;; Procedure
;;   hexturn-move-straight?
;;
;; Purpose
;;   Determine whether a puzzle play is a straight-line move
;;
;; Parameters
;;   next, a pair of integers
;;   current, a pair of integers
;;   previous, a pair of integers
;;
;; Produces
;;   straight?, a boolean
;;
;; Preconditions
;;   next, current, and previous represent valid tile locations for the same
;;     hexturn puzzle.
;;
;; Postconditions
;;   straight? is #t if the move from current to next, with previous before,
;;    represents one of the three possible straight moves on the hex grid.
;;
(define hexturn-move-straight?
  (lambda (next current previous)
    (or (and (= (car next) (car current)) ; all in the same row
             (= (car next) (car previous)))
        (and (= 2 (abs (- (cdr next) (cdr previous)))) ; cols differ maximally
             (not (= (car next) (car previous)))))))   ; but not on same row

;;
;; Procedure
;;   hexturn-move-sharp?
;;
;; Purpose
;;   Determine whether a puzzle play is a sharp-turn move
;;
;; Parameters
;;   next, a pair of integers
;;   current, a pair of integers
;;   previous, a pair of integers
;;
;; Produces
;;   sharp?, a boolean
;;
;; Preconditions
;;   next, current, and previous represent valid tile locations for the same
;;     hexturn puzzle.
;;
;; Postconditions
;;   sharp? is #t if the move from current to next, with previous before,
;;    represents one of the six possible sharp moves on the hex grid.
;;
(define hexturn-move-sharp?
  (lambda (next current previous)
    (or (and (= (car next) (car previous))       ; start and end in same row
             (not (= (car next) (car current)))) ; with current different, v|^
        (= 1 (abs (- (cdr next) (cdr previous))))))); start/end in adj cols <|>

;;
;; Procedure
;;   hexturn-move-gentle?
;;
;; Purpose
;;   Determine whether a puzzle play is a gentle-turn move
;;
;; Parameters
;;   next, a pair of integers
;;   current, a pair of integers
;;   previous, a pair of integers
;;
;; Produces
;;   gentle?, a boolean
;;
;; Preconditions
;;   next, current, and previous represent valid tile locations for the same
;;     hexturn puzzle.
;;
;; Postconditions
;;   gentle? is #t if the move from current to next, with previous before,
;;    represents one of the eight possible gentle moves on the hex grid.
;;
(define hexturn-move-gentle?
  (lambda (next current previous)
    (or (and (= 2 (abs (- (car next) (car previous))))
             (= (cdr next) (cdr previous)))
        (and (= 3 (abs (- (cdr next) (cdr previous))))
             (= 1 (abs (- (car next) (car previous))))))))

;;
;; Procedure
;;   hexturn-move-direction
;;
;; Purpose
;;   Determine direction of a puzzle play  move
;;
;; Parameters
;;   next, a pair of integers
;;   current, a pair of integers
;;   previous, a pair of integers
;;
;; Produces
;;   move, a symbol
;;
;; Preconditions
;;   next, current, and previous represent valid tile locations for the same
;;     hexturn puzzle.
;;
;; Postconditions
;;   move is one of straight, sharp, or gentle, the direction indicated by
;;     predicates hexturn-move-straight?, hexturn-move-sharp?, and
;;     hexturn-move-gentle?
;;
(define hexturn-move-direction
  (lambda (next current previous)
    (let ([straight? (hexturn-move-straight? next current previous)]
          [sharp? (hexturn-move-sharp? next current previous)]
          [gentle? (hexturn-move-gentle? next current previous)])
      ;; Assert/Verify mutually exclusive and exhaustive
      (cond
        [(or (and straight? sharp?)
             (and straight? gentle?)
             (and sharp? gentle?))
         (error "Mutual exclusion violation for moves "
                (list next current previous))]
        [(not (or straight? sharp? gentle?))
         (error "Exhaustiveness violation for moves "
                (list next current previous))]
        [straight? 'straight]
        [sharp?    'sharp]
        [gentle?   'gentle]
        [else
         (error "Invalid state: mutual exclusion or exhaustiveness violation "
                (list next current previous))]))))

;;
;; Procedure
;;   hexturn-color
;;
;; Purpose
;;   Get the color of the current tile in a board
;;
;; Parameters
;;   state, a hexturn state
;;
;; Produces
;;   color, a symbol
;;
;; Preconditions
;;   [No additional.]
;;
;; Postconditions
;;   color is the symbol white, yellow, orange or green, depending on the board in
;;     the given state and the current tile position
(define hexturn-color
  (lambda (state)
    (let* ([num-rings (hexturn-num-rings state)]
           [board (hexturn-board state)]
           [current-tile (car (hexturn-moves state))])
      (hexturn-board-color num-rings board current-tile))))

;;
;; Procedure
;;   hexturn-board-color
;;
;; Purpose
;;   Get the color of a tile in a board
;;
;; Parameters
;;    num-rings, an integer
;;    board, a hexturn puzzle board
;;    tile, a pair of integers
;;
;; Produces
;;   color, a symbol
;;
;; Preconditions
;;   num-rings > 0
;;   board is a vector-of-vectors of appropriate dimension for num-rings
;;   tile contains integers appropriate to board of size num-rings
;;
;; Postconditions
;;   color is the symbol white, yellow, or orange, depending on the board in
;;     the given board and the tile position

(define hexturn-board-color
  (lambda (num-rings board tile)
    (let* ([tile-row (car tile)]
           [tile-col (cdr tile)]
           [full-grid-row (+ tile-row (- num-rings 1))]
           [packed-grid-row (quotient full-grid-row 2)]
           [packed-grid-col (+ tile-col (* 2 (- num-rings 1)))])
      (vector-ref (vector-ref board packed-grid-row)
                  packed-grid-col))))

;;
;; Procedure
;;   random-hexturn-puzzle
;;
;; Purpose
;;   Generate a solveable hexturn puzzle via a random walk
;;
;; Parameters
;;   num-rings, an integer
;;
;; Produces
;;   state, a hexturn puzzle state
;;
;; Preconditions
;;   num-rings > 0
;;
;; Postconditions
;;   state has a solution (not necessarily unique)
;;
(define random-hexturn-puzzle
  (lambda (num-rings)
    (let* ([path ; Take a random walk on the board until no moves are available
            (let walk ([so-far (list (cons 0 0))]) ; start in the center
              (let* ([neighbors (hexturn-tile-neighbors num-rings (car so-far))]
                     [valid-move? (lambda (tile) (not (member tile so-far)))]
                     [moves (filter valid-move? neighbors)])
                (if (null? moves) ; No available moves. 
                    so-far        ; Finish and return.
                    (walk (cons (list-ref moves (random (length moves)))
                                so-far)))))] ; Add random valid move
           [row-length (+ 1 (* 4 (- num-rings 1)))] ; Length of each row's vector
           [board ; Initial packed board vec-of-vecs, with entries set to #f
            (let ([rows (make-vector num-rings)])
              (do ([row 0 (+ 1 row)])
                  [(= row num-rings) rows]
                (vector-set! rows row (make-vector row-length #f))))]
           [board-set! ; Board mutator for tile indices
            (lambda (tile val)
              (let* ([full-row (+ (car tile) (- num-rings 1))]
                    [packed-row (quotient full-row 2)]
                    [packed-col (+ (cdr tile) (* 2 (- num-rings 1)))])
                (vector-set! (vector-ref board packed-row) packed-col val)))]
           ) ; let* bindings
      
      (board-set! (cons 0 0) 'white) ; Set start white
      (board-set! (car path) 'green) ; Set goal green
      
      (do ((moves path (cdr moves))) ; Set according to move direction
          ((null? (cddr moves)) board)
        (let* ([turn ; Calculate the move direction based on three move window
                (hexturn-move-direction (car moves)
                                        (cadr moves)
                                        (caddr moves))]
               [color ; Determine color based on move
                (cond
                 [(equal? turn 'sharp)  'orange]
                 [(equal? turn 'gentle) 'yellow]
                 [(zero? (random 2))    'orange] ; Flip coin if straight
                 [else                  'yellow])])
          (board-set! (cadr moves) color)))
      
      (do ((i (- (- num-rings 1)) ; Set all others randomly
              (+ 1 i)))
          ((> i (- num-rings 1))
           board)
        (do ((j (- (- (* 2 (- num-rings 1))
                      (abs i)))
                (+ j 2)))
            ((> j (- (* 2 (- num-rings 1))
                     (abs i)))
             board)
          (when (not (hexturn-board-color num-rings board (cons i j)))
            (board-set! (cons i j)
                        (if (zero? (random 2))
                            'orange
                            'yellow)))))

      (list (list (cons 0 0)) ; Start in center
            board      ; Return mutated board
            (car path); Goal was last move
            num-rings))))

(define hexturn-board-print
  (let ([print-spaces
         (lambda (n)
           (do ((i n (- i 1)))
               ((zero? i))
             (display " ")))])
    (lambda (num-rings board)
      (do ((i (- (- num-rings 1))
              ( + 1 i)))
          ((> i (- num-rings 1)))
        (print-spaces (abs i)) ; leading spaces of row i
        (do ((j (- (- (* 2 (- num-rings 1))
                      (abs i)))
                (+ j 2)))
            ((> j (- (* 2 (- num-rings 1))
                     (abs i))))
          ;(display (list i j))(newline)
          (let ([color (hexturn-board-color num-rings board (cons i j))])
            (display
             (cond [(equal? color 'white)  "W "]
                   [(equal? color 'yellow) "Y "]
                   [(equal? color 'orange) "O "]
                   [(equal? color 'green)  "G "]
                   [else                   "? "])))); indicates errors elsewhere
        (newline)))))
        
           
