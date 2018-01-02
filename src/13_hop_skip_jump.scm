; Chapter 13: Hop, Skip, and Jump


; recall the function intersect from a-ways back.
; rewritten, following the 12th Commandment:
(define intersect
  (lambda (set1 set2)
    (letrec
      ((I (lambda (set)
            (cond
              ((null? set) '())
              ((member? (car set) set2)
               (cons (car set) (I (cdr set))))
            (else
              (I (cdr set)))))))
      (I set1))))

(intersect '(futomaki and temaki) '(futomaki or suhi))

; intersectall intersects a list of sets
(define intersectall
  (lambda (lset)
    (cond
      ((null? lset) (quote ()))           ; we only have to ask this question once, b/c
      ((null? (cdr lset)) (car lset))     ;   we never recur when (cdr lset) is null
    (else
      (intersect (car lset)
                  (intersectall (cdr lset)))))))

; here, we do not make any assumptions about lset,
; AND we only need to ask (null? lset) once.
; all recursions call the helper function A
(define intersectall-letrec
  (lambda (lset)
    (letrec
      ((A (lambda (lset)
            (cond
              ((null? (cdr lset)) (car lset))
            (else
              (intersect (car lset)
                         (A (cdr lset))))))))
      (cond
        ((null? lset) '())    ; note again how this question is NOT a part of A
      (else (A lset))))))

(intersectall-letrec '((a b c d)))  ; => '(a b c d)
(intersectall-letrec '((a b c d) (b c d e) (c d e f)))  ; => '(c d)
(intersectall-letrec '((3 mangos and) (3 kiwis and) (3 hamburgers)))  ; => '(3)

; N.B.: we could use whatever name we want for the helper function
; inside of the letrec, b/c letrec hides definitions, and the names
; only matter inside of letrec

; note this case:
(intersectall-letrec '((3 mangos and) () (3 diet hamburgers)))  ; => '()
; we do a lot of extra work here, even though we should know to return '() immediately

; we can now introduce letcc, a.k.a. "call-with-current-continuation"
; for this macro, see http://community.schemewiki.org/?seasoned-schemer
; i've defined the macro in 'helpers.scm'

; this uses the canoncical scheme version. I define both here, but
; I will use the letcc macro going forward
(define intersectall-letcc-call
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
        (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                (else
                  (intersect (car lset)
                             (A (cdr lset))))))))
          (cond
            ((null? lset) '())
          (else (A lset))))))))

(define intersectall-letcc
  (lambda (lset)
    (letcc hop
      (letrec
        ((A (lambda (lset)
              (cond
                ((null? (car lset))   ; letcc gives us the ability to immediately return '() if we encounter '() in lset
                  (hop (quote ())))
                ((null? (cdr lset))
                  (car lset))
              (else
                (intersect (car lset)
                  (A (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
      (else (A lset)))))))



; not quite finished -- intersectall that returns abruptly and promptly
(define intersectall-ap
  (lambda (lset)
    (letcc hop
        (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                    (hop (quote ())))
                  ((null? (cdr lset))
                    (car lset))
                (else (I (car lset)
                         (A (cdr lset)))))))
           (I ...))
          (cond
            ((null? lset) (quote ()))
          (else (A lset)))))))


; NOTE: one reason to do this is b/c the value of a never changes.
; see 12th commandment
(define rember-letrec
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
                        (R (cdr lat))))))))
      (R lat))))

; rember-beyond-first removes the first occurence of a and everything that comes after it.
; it differs from rember in only one answer
(define rember-beyond-first
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? (car lat) a) (quote ()))    ; this is the different answer
            (else (cons (car lat)
                        (R (cdr lat))))))))
      (R lat))))

; remove everything up to the last occurence of 'a
(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? (car lat) a)
               (skip (R (cdr lat))))        ; this is what drops everything up to the last 'a
            (else (cons (car lat)
                        (R (cdr lat))))))))
      (R lat)))))

; (define lat '(cookies chocolate mints caramelt delight ginger snaps cookies gingerbread mint chocolate chip))
; (rember-upto-last 'cookies lat)  => '(gingerbread mint chocolate chip)
