; Chapter 15: The Difference Between Men and Boys...

; what is the *value* of
(define x
  (cons 'chicago
    (cons 'pizza '())))

; => '(chicago pizza)

(set! x 'gone)
(define y 'gone)
(eq? x y) ; => #t

; redefine x:
(set! x 'skins)
(define gourmet
  (lambda (food)
    (cons food
      (cons x '())))) ; x => 'skins

(gourmet 'onion)  ; => '(onion skins)

; re-redefine x:
(set! x 'rings)
(gourmet 'onion)  ; => '(onion rings)

; in this function we will always remember the
; last value passed to gourmand
(define gourmand
  (lambda (food)
    (set! x food)         ; expr 1
    (cons food            ; expr 2
      (cons x '()))))

(gourmand 'potato)  ; => '(potato potato)
x ; => 'potato

(define diner
  (lambda (food)
    (cons (quote milkshake)
      (cons food
        (quote ())))))
; dinerR will "remember" the last food it ate too
(define dinerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
      (cons food
        (quote ())))))

(dinerR (quote pecanpie))
x ; => 'pecanpie
(gourmand (quote onion))
x ; => 'onion

; we have a conflict -- since x is set! by these functions,
; it can refer to something we don't expect. Watch out for
; side-effects!

; we can try to avoid this problem like so:
(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food
        (cons x '())))))

; this is almost the same as having two definitions:
(define x_1 'minestrone)
(define omnivore-no-let
  (lambda (food)
    (set! x_1 food)
    (cons food
      (cons x_1 '()))))

; ^ treat the x_1 thing as an imaginary name
; TODO(ian): is this the same as environments??

(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food
        (cons x '())))))

; same as:
(define x_2 'minestrone)
(define gobbler-no-let
  (lambda (food)
    (set! x_2 food)
    (cons food
      (cons x_2 '()))))

; NOTE: missing pages 100-101

; breaking the 17th Commandment for clarity:
(define food 'none)
(define glutton
  (lambda (x)
    (set! food x)
    (cons (quote more)
      (cons x
        (cons (quote more)
          (cons x '()))))))

(glutton 'garlic) ; => '(more garlic more garlic)
food  ; => 'garlic
x     ; => 'onion

; ^ in case you forgot, gourmand set! x to 'onion (see above)

; if we wanted to swap the values of x and food, would this work?
(define chez-nous-bad
  (lambda ()
    (set! food x)
    (set! x food)))

; This will fail. We need a temp value for the swap.

(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(chez-nous)
food  ; => 'onion
x     ; => 'garlic
