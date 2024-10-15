#lang racket

; Exam 2023.09.12

; (multifun <list of functions><list of parameters><list of bodies>)
; (multifun (f g) (x) ((+ x x x)(* x x))

#(define-syntax multifun1
  (syntax-rules ()
    ((_ functions parameters bodies ...)
     (begin
       (let loop ((f-current (car functions))
                  (f-rest (cdr functions))
                  (b-current (car bodies))
                  (b-rest (car bodies)))
         (define (f-current parameters) (b-current))
         (unless (or (null? f-rest)(null? b-rest))
           (loop ((car f-rest)
                  (cdr f-rest)
                  (car b-current)
                  (cdr b-current)))))))))



(define-syntax multifun
  (syntax-rules ()
    ((_ (f)(args ...)(body))
      (define (f args ...) body))
    ((_ (f . funcs)(args ...)(body . bodies))
      (begin
        (multifun (f) (args ...) (body))
        (multifun funcs (args ...) bodies)))))

(multifun (f) (x) ((+ x x x)))
(f 2)


(multifun (g h)
          (x y)
          ((+ x x x)
           (+ y 2)))
(g 2 3)
(h 2 3)

; An alternative
(define-syntax multifun2
  (syntax-rules ()
    ((_ (f)(args ...)(body))
      (define (f args ...) body))
    ((_ (f funcs ...)(args ...)(body bodies ...))
      (begin
        (multifun2 (f) (args ...) (body))
        (multifun2 (funcs ...) (args ...) (bodies ...))))))

(multifun2 (k l)
          (x y)
          ((+ x x x)
           (+ y 2)))
(k 2 3)
(l 2 3)