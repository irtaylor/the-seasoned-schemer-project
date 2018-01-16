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
