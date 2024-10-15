#lang racket

(define storage '())

(define-syntax store-cc
  (syntax-rules ()
    ((_ body ...)
     (call/cc
      (lambda(c)
        body ...
        (set! storage (cons c storage)))))))

(define (run-cc)
  (unless (empty? storage)
    (let ((c (car storage)))
      (set! storage (cdr storage))
      (c))))


(define (test)
 (define x 0)
 (store-cc
 (displayln "here")
 (set! x (+ 1 x)))
 (displayln x)
 (set! x (+ 1 x))
 x)
(test)

(newline)

(run-cc)