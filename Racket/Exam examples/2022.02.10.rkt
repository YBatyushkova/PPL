#lang racket
(define (r x y . s)
  (set! s (if (cons? s) (car s) 1))
  (lambda ()
    (if (< x y)
        (let ((z x))
          (set! x (+ s x))
          z)
        y)))

(r 1 '(10 2))