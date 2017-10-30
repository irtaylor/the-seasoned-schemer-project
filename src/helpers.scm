; Note: this `define...lambda` structure essentially names the funtion
; with `define`, and then specifies the parameters in the anonymous `lambda`
; this is nice b/c it's like a sentence: "The name of this thing is defined as <define>...
; ... and its body is a function that does <lambda>."
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; recursively check each element of a list to determine if it is an atom
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; ask: is `a` a member of `lat`?
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
        (member? a (cdr lat)))))))

; return final element of list
(define finalElem
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((null? (cdr lat)) (car lat))
      (else (finalElem (cdr lat))))))

; reverse elements of lat
(define reversify
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (reversify (cdr lat)) (car lat))))))

(define-syntax letcc
 (syntax-rules ()
   ((letcc var body ...)
    (call-with-current-continuation
      (lambda (var)  body ... )))))
(define-syntax try
 (syntax-rules ()
   ((try var a . b)
    (letcc success
      (letcc var (success a)) . b))))
