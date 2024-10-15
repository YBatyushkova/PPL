#lang racket

#|
(define-syntax list-to-compose
  (syntax-rules ()
    ((_ lst ...)
     (unless (empty? lst ...)
       (lambda (x)
         (foldr apply x lst ...))))))
|#


(define (f x)
  (+ 1 x))
(define (g x)
  (+ 2 x))
(define (h x)
  (+ 3 x))

(define (list-to-compose lst)
  (lambda (x)
    (foldr (lambda (y acc) (y acc)) x lst)))

((list-to-compose (list f g h)) 5)

