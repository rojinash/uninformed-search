;;; Authors:
;;;   Janet Davis
;;;   Samuel A. Rebelsky
;;;   Jerod Weinman

;;; Procedure:
;;;   list-insertion-sort
;;; Parameters:
;;;   lst, a list to be sorted
;;;   may-precede?, a binary predicate
;;; Purpose:
;;;   Sort lst.
;;; Produces:
;;;   sorted, a list.
;;; Preconditions:
;;;   may-precede? can be used with the elements of lst. That is for
;;;     all values a and b in lst, (may-precede? a b) successfully
;;;     returns a truth value.
;;;   may-precede? is transitive.  That is, for all values a, b, and 
;;;     c in lst, if (may-precede? a b) and (may-precede? b c), then
;;;     (may-precede? a c).
;;;   may-precede? is sensible.  That is, for all values a and b,
;;;     either (may-precede? a b), (may-precede? b a), or both.
;;; Postconditions:
;;;   sorted is sorted using may-precede?.  That is, for all i 
;;;     such that 0 <= i < (- (length lst) 1),
;;;     (may-precede? (list-ref sorted i)
;;;                   (list-ref sorted (+ i 1)))
;;;   sorted is a permutation of lst.
(define list-insertion-sort
  (lambda (lst may-precede?)
    (letrec ((insert
              (lambda (lst val)
                (cond
                  ((null? lst)
                   (list val))
                  ((may-precede? val (car lst))
                   (cons val lst))
                  (else
                   (cons (car lst) (insert (cdr lst) val))))))
             (kernel
              (lambda (unsorted sorted)
                (if (null? unsorted) 
                    sorted
                    (kernel (cdr unsorted) (insert sorted (car unsorted)))))))
      (kernel lst null))))



;;; Procedure:
;;;   list-keyed-insertion-sort
;;; Parameters:
;;;   lst, a list
;;;   get-key, a procedure
;;;   may-precede?, a binary predicate
;;; Purpose:
;;;   Sort lst.
;;; Produces:
;;;   sorted, a list
;;; Preconditions:
;;;   get-key? can be applied to each element of lst.
;;;   may-precede? can be used with the values returned by get-key. That is 
;;;     for all values a and b in lst, (may-precede? (get-key a) (get-key b)) 
;;;     successfully returns a truth value.
;;;   may-precede? is transitive.  That is, for all keys a, b, and 
;;;     c, if (may-precede? a b) and (may-precede? b c), then
;;;     (may-precede? a c).
;;;   may-precede? is sensible.  That is, for all keys a and b,
;;;     (may-precede? a b) holds, (may-precede? b a) holds, or both
;;;     hold.
;;; Postconditions:
;;;   sorted is sorted by key using may-precede?.  That is, for all i 
;;;     such that 0 <= i < (- (length lst) 1),
;;;     (may-precede? (get-key (list-ref sorted i))
;;;                   (get-key (list-ref sorted (+ i 1))))
;;;   sorted is a permutation of lst.
(define list-keyed-insertion-sort
  (lambda (lst get-key may-precede?)
    (list-insertion-sort lst
                         (lambda (v1 v2)
                           (may-precede? (get-key v1) (get-key v2))))))
