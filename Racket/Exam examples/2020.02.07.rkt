#lang racket


(define-syntax each-until
  (syntax-rules (in until :)
    ((_ var in lst until pred : body ...)
     (let loop ((rest lst))
       (unless (null? rest)
         (let ((var (car rest)))
           (unless pred
             (begin
               body ...
               (loop (cdr rest))))))))))

(each-until x in '(1 2 3 4)
            until (> x 3) :
            (display (* x 3))
            (display " "))
