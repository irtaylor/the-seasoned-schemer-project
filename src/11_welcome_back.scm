; Chapter 11: Welcome Back to the Show

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
    (else (or (eq? a (car lat))
              (member? a (cdr lat)))))))

; is a the first element of lat?
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
    (else (eq? (car lat) a)))))

; two-in-a-row? determines whether any atom occurs twice in a row
; in a list of atoms.
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
    (else
      (or (is-first? (car lat) (cdr lat))   ; this or branch indicates that is-first and two-in-a-row? make decisions about whether or not to continue the search.
          (two-in-a-row? (cdr lat)))))))

; perhaps we should change the above two functions, such that is-first?
; is entirely responsible for deciding whether or not to continue the search...
(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
    (else (or (eq? (car lat) a)
              (two-in-a-row-b? lat))))))

(define two-in-a-row-b?
  (lambda (lat)
    (cond
      ((null? lat) #f)
    (else
      (is-first-b? (car lat) (cdr lat))))))

; can we have two-in-a-row? recur directly on itself
; this version of two-in-a-row? takes two arguments.
; note that both arguments change, although we only ask questions on the second argument.
(define two-in-a-row-c?
  (lambda (preceeding lat)
    (cond
      ((null? lat) #f)
    (else (or (eq? (car lat) preceeding)
              (two-in-a-row-c? (car lat) (cdr lat)))))))

; and now we can define two-in-a-row? that takes a single argument,
; but hands the decision off to two-in-a-row-c?
(define two-in-a-row-final?
  (lambda (lat)
    (cond
      ((null? lat) #f)
    (else (two-in-a-row-c? (car lat) (cdr lat))))))

; sum-of-prefixes takes a list of numbers, tup, and generates a new list where each
; element is the sum of the previous elements of tup.
; e.g. (sum-of-prefixes '(2 1 9 17 0)) => '(2 3 12 29 29)
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define sum-of-prefixes-b
  (lambda (sonssf tup)                ; sonssf is an accumulator, the "sum of numbers seen so far"
    (cond
      ((null? tup) (quote ()))
    (else (cons (+ sonssf (car tup))
                (sum-of-prefixes-b (+ sonssf (car tup))
                                   (cdr tup)))))))

; scramble takes a non-empty tup in which no number is greater than its own index
; and returns a tup of the same length. Each number in the argument is treated as a
; backward index from its own position to a point earlier in the tup. The result at
; each position is found by counting backward from the NEXT position to this index.

; i.e. from the original tup, pick an element. starting with that element, count
; backward by the value of the selected element. the final index is the mapped
; value in the returned list.

'(1 1 1 3 4 2 1 1 9 2)      ; e.g. for the last element 2, count backwards 2 elements (2 9)
'(1 1 1 1 1 4 1 1 1 9)

; pick (ch 4, Little Schemer) picks the nth element from the list
(define pick
  (lambda (n lat)
    (cond
      ((= 1 n) (car lat))
    (else (pick (- n 1) (cdr lat))))))

; scramble-b takes a tup and its reversed proper prefix. Unless the tup is empty,
; it constructs the reverse of the complete prefix and uses the first element of tup
; as a backward index into this list
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) (quote ()))
    (else
      (cons (pick (car tup)
              (cons (car tup) rev-pre))
            (scramble-b (cdr tup)
                        (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))
