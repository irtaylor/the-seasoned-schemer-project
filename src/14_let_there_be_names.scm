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
; (leftmost-let '(((()) ()))) => '()
