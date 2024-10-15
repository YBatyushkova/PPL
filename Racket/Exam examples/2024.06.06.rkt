#lang racket

(define-syntax let-cond
  (syntax-rules ()
    ((_ ((condition ((var val) ...) then-body)...) else-body)
     (cond
       (condition
        (let ((var val) ...)
          then-body)) ...
       (else else-body)))))

(let-cond
 [((> 5 13)
   [(a 10) (b 20)]
   (+ a b))
           
  ((= 5 5)
   [(c 3) (d 4)]
   (+ c d))]

 "all conditions false") 