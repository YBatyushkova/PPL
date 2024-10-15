#lang racket

#|
(define-syntax <-
  (syntax-rules ()
    ((var _ first-value second-value)
     (cons (first-value) (second-value)))))
(x <- 12 3)
|#

(define-syntax block
  (syntax-rules (then where <-)
    ((_ (first-body ...)
        then
        (second-body ...)
        where (var <- a b)...)
     (begin
       (let ((var a) ...)
         first-body ...)
       (let ((var b) ...)
         second-body ...)))))


(block ((displayln (+ x y))
        (displayln (* x y))
        (displayln (* z z)))
       then ((displayln (+ x y))
             (displayln (* z x)))
       where (x <- 12 3)(y <- 8 7)(z <- 3 2))