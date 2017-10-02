; Chapter 12: Take Cover

; our friend, the applicative order Y-combinator
(define Y
    (lambda (f)
      ((lambda (x) (x x))
        (lambda (x) (f (lambda (y) ((x x) y)))))))

; introducing letrec
(define multirember-letrec
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
          (cond
            ((null? lat) '())
            ((eq? a (car lat)) (mr (cdr lat)))
          (else
            (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

; ((letrec ((mr ...)) mr) lat)  => the result of applying mr to lat


; also (letrec ((mr ...)) (mr lat)), makes the above a little easier:
(define multirember-letrec-2
  (lambda (a lat)
    (letrec
      ((mr (lambda (lat)
        (cond
          ((null? lat) '())
          ((eq? a (car lat)) (mr (cdr lat)))
        (else
          (cons (car lat) (mr (cdr lat))))))))
      (mr lat))))

; in the above function, a does not change, but lat will change
; as the recursion progresses. letrec means "let with recursive definitions".
; in this case, the bound function mr is recursive.

; defining multirember-f in the style of Chapter 8
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat))
            ((multirember-f test?) a (cdr lat)))
      (else (cons (car lat)
                  ((multirember-f test?)
                    a (cdr lat))))))))

; multirember-f takes a function test? and returns a new function.
; we can use letrec to express this as well:
(define multirember-f-letrec
  (lambda (test?)
    (letrec
      ((m-f
          (lambda (a lat)
            (cond
              ((null? lat) (quote ()))
              ((test? a (car lat))
                (m-f a (cdr lat)))
            (else
              (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))
