#lang racket

(define-syntax cobol-fold
  (syntax-rules (direction -> <- from data exec using)
    ((_ direction -> from i data elem ... (exec body ...) using acc lst)
     (foldr (lambda (acc lst) body ...) i (list elem ...)))
    ((_ direction <- from i data elem ... (exec body ...) using acc lst)
     (foldl (lambda (acc lst) body ...) i (list elem ...)))))


(define numbers '(1 2 3 4 5))
(foldl + 0 numbers)

(cobol-fold direction -> from 1 data 1 2 3 4 5 6
            (exec (displayln y)
                  (+ x y))
            using x y)

(cobol-fold direction <- from 0 data 1 2 3 4 5
            (exec (+ x y))
            using x y)