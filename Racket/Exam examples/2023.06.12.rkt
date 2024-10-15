#lang racket


(define (fold-left-right f i L)
  (let loop ((left i)
             (right (lambda(x) x))
             (rest L))
    (if (null? rest)
        (cons left (right i))

        (loop (f (car rest) left)
              (lambda (x) (right (f (car rest) x)))
              (cdr rest)))))

(fold-left-right string-append "" '("a" "b" "c"))