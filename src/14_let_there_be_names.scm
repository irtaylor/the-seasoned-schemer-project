; Chapter 14: Let There Be Names

; leftmost extracts the leftmost atom from a list of S-expr
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
    (else (leftmost (car l))))))

; (leftmost '(((a) b) (c d))) => 'a
; (leftmost '(((() a) ()))) => error, b/c leftmost is empty list

; leftmost-2 does not have the above problem
(define leftmost-2
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
    (else
      (cond
        ((atom? (leftmost-2 (car l)))
         (leftmost-2 (car l)))              ; notice here we're needlessly repeating the previous recursions
      (else (leftmost-2 (cdr l))))))))

; (leftmost-2 '(((a) b) (c d)))
; (leftmost-2 '(((() a) ())))
; (leftmost-2 '(((()) ()))) => '()

(define leftmost-let
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
    (else
      (let ((a (leftmost-let (car l))))
        (cond
          ((atom? a ) a)
        (else (leftmost-let (cdr l)))))))))

; (leftmost-let '(((a) b) (c d)))
; (leftmost-let '(((() a) ())))
; (leftmost-let '(((()) ())))

(define rember1*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (cdr l))
        (else (cons (car l) (rember1* a (cdr l))))))
    (else
      (cond
        ((equal? (rember1* a (car l))
          (car l))
        (cons (car l) (rember1* a (cdr l))))
      (else (cons (rember1* a (car l))
              (cdr l))))))))

(rember1* 'salad '((Swedish rye) French (mustard salad turkey) salad))

(define rember1*-letrec
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else
                   (cons (car l) (R (cdr l))))))
              (else
                (cond
                  ((equal? (R (car l)) (car l)) ; if the list with 'a' removed doesn't change
                   (cons (car l) (R (cdr l))))  ; then recurse
                  (else
                    (cons (R (car l)) (cdr l))))))))) ; otherwise remove 'a'
      (R l))))

(rember1*-letrec 'salad '((Swedish rye) French (mustard salad turkey) salad))

(define rember1*-let
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else
                   (cons (car l) (R (cdr l))))))
              (else
                (let ((av (R (car l))))
                  (cond
                    ((equal? (car l) av)         ; if the list with 'a' removed didn't change
                     (cons (car l) (R (cdr l)))) ; then recurse
                    (else
                      (cons av (cdr l))))))))))  ; otherwise remove 'a'
      (R l))))

; TODO: Complete this chapter later...
