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

; (multirember-f eq?)   => multirember-letrec-3:
(define multirember-letrec-3
  (letrec
    ((mr
      (lambda (a lat)
        (cond
          ((null? lat) (quote ()))
          ((eq? a (car lat))
            (mr a (cdr lat)))
        (else
          (cons (car lat)
                (mr a (cdr lat))))))))
    mr))

; member? checks if a is in lat. note again that a doesn't change
; during the natural recursions, so we can use the 12th Commandment.
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
    (else (member? a (cdr lat))))))

(define member-letrec?
  (lambda (a lat)
    (letrec
      ((yes?
          (lambda (l)
            (cond
              ((null? l) #f)
              ((eq? a (car l)) #t)
            (else
              (yes? (cdr l)))))))
      (yes? lat))))

; remember that a set is a list where no atom occurs twice.
; remember also, the the union function merges too sets into one set.
; and once again, observe that set2 doesn't change for any of the recursions.
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
          (union (cdr set1) set2))
    (else (cons (car set1)
                (union (cdr set1) set2))))))

(define union-letrec
  (lambda (set1 set2)
    (letrec
      ((U
        (lambda (set)
          (cond
            ((null? set) set2)
            ((member? (car set) set2)
                (U (cdr set)))
          (else (cons (car set)
                        (U (cdr set))))))))
      (U set1))))

; Remember: the function U knows about set1 b/c it was defined using letrec.
; U thus knows about everything that union knows about.

; U also knows about the function member?,  but the order of arguments
; to member? matters. if we said instead:

; (define member?
;   (lambda (lat a)...))

; then union would get confused! We should find a way to get rid of
; union's dependence on the order of arguments in member?

(define union-letrec-2
  (lambda (set1 set2)
    (letrec
      ((U
        (lambda (set)
          (cond
            ((null? set) set2)
            ((M? (car set) set2)
                (U (cdr set)))
          (else (cons (car set)
                        (U (cdr set)))))))
       (M?
         (lambda (a lat)
          (cond
            ((null? lat) #f)
            ((eq? a (car lat)) #t)
          (else (M? a (cdr lat)))))))
      (U set1))))

; this is also a better option, but now the definition of M?
; ignores the 12th Commandment.

(define union-letrec-3
  (lambda (set1 set2)
    (letrec
      ((U
        (lambda (set)
          (cond
            ((null? set) set2)
            ((M? (car set) set2)
                (U (cdr set)))
          (else (cons (car set)
                        (U (cdr set)))))))
       (M?
         (lambda (a lat)
            (letrec
              ((N?
                (lambda (lat)
                  (cond
                    ((null? lat) #f)
                    ((eq? (car lat) a) #t)
                  (else (N? (cdr lat)))))))
              (N? lat)))))
      (U set1))))


; remember that two-in-a-row? uses helper functions, which could be protected
(define two-in-a-row?
  (lambda (lat)
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
            (else (or (eq? (car lat) a)
                      (W (car lat) (cdr lat))))))))
      (cond
        ((null? lat) #f)
      (else (W (car lat) (cdr lat)))))))

; remember that two-in-a-row? uses helper functions, which could be protected
(define two-in-a-row-b?
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
            (else (or (eq? (car lat) a)
                      (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
      (else (W (car lat) (cdr lat)))))))

(define sum-of-prefixes
  (lambda (tup)
    (letrec
      ((S (lambda (sss tup)
            (cond
              ((null? tup) (quote ()))
            (else
              (cons (+ sss (car tup))
                    (S (+ sss (car tup))
                       (cdr tup))))))))
      (S 0 tup))))

(define scramble
  (lambda (tup)
    (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) (quote ()))
            (else (cons (pick (car tup)
                          (cons (car tup) rp))
                        (P (cdr tup)
                           (cons (car tup) rp))))))))
      (P tup (quote ())))))
