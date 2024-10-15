#lang racket

#| Define the construct define-with-types, that is used to define
a procedure with type constraints, both for the parameters and for
the return value. The type constraints are the corresponding type predicates,
e.g. number? to check if a value is a number.
If the type constraints are violated, an error should be issued.

E.g.
(define-with-types (add-to-char : integer? (x : integer?) (y : char?))
  (+ x (char->integer y)))
defines a procedure called add-to-char, which takes an integer and a character,
and returns an integer.
|#


(define-syntax define-with-types
  (syntax-rules (:)
    ((_ (proc : proc-pred (var : var-pred) ...) proc-body ...)
     (define (proc var ...)
       (if (and (var-pred var) ...)
           (let ((res (begin
                        proc-body ...)))
                 (if (proc-pred res)
                     res
                     (error "Wrong output")))
           (error "Wrong input"))))))


(define-with-types
  (add-to-char : integer? (x : integer?) (y : char?))
  (+ x (char->integer y)))

(add-to-char 2 '(w))
#|
(define-syntax define-with-types
  (syntax-rules (:)
    ((_ (proc : proc-pred (var : var-type) ...) proc-body)
     (if (and (var-pred var) ...)
         (define (proc var ...) proc-body)
         (error "Wrong variable type")))))     
|#        