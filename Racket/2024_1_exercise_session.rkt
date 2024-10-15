#lang racket

(define (hello)
  "hi")

(define (len L)
  (define (l L n)
    (if (null? L)
        n
        (l (cdr L) (+ 1 n))))
  (l L 0))
        
(define (pref n L)
  (define (pref-help n L r)
    (if (= n 0)
        r
        (pref-help (- n 1) (cdr L) (append r (list (car L))))))
  (pref-help n L '()))

(define (ref k L)
  (if (= k 0)
      (car L)
      (ref (- k 1) (cdr L))))

; (range 4) '(0 1 2 3 4)
; (range 1 4) '(1 2 3 4)
(define (range-with-step s e step)
  (if (= s e)
      (list e)
      (cons s (range-with-step (+ s step) e step))))

; VARIABLE NUMBER OF PARAMETERS
(define (range x . y)
  (define (r s e)
    (if (= s e)
      (list e)
      (cons s (r (+ s 1) e))))
  (if (null? y)
      (r 0 x)
      (r x (car y))))

; THUNK
(define (test-while)
  (let ((x 0))
    (while (lambda () (< x 10))
           (lambda ()
             (displayln x)
             (set! x (+ x 1))))))

(define (while c b)
  (when (c)
    (b)
    (while c b)))


(define (rev L)
  (if (null? L)
      '()
      (append (rev (cdr L)) (list (car L)))))

(define (flatten L)
  (if (null? L)
      '()
      (if (list? (car L))
          (append (flatten (car L)) (flatten (cdr L)))
          (cons (car L) (flatten (cdr L))))))          