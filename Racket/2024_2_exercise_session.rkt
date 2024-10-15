#lang racket

(struct person
  (name
   (age #:mutable)))

(define p1 (person "Ada" 22))
(define p2 "Bob")

(set-person-age! p1 25)

(person-age p1)


(struct node
  ((value #:mutable)))

(struct branch-node node
  (left right))

(define (leaf? n)
  (and (node? n) (not (branch-node? n))))

(define my-tree
  (branch-node
   2
   (branch-node
    3
    (node 4)
    (node 2))
   (node 1)))

; show
(define (print-tree n)
  (displayln (node-value n))
  (unless (leaf? n)
           (print-tree (branch-node-left n))
           (print-tree (branch-node-right n))))

; functor
(define (tree-apply f n)
  (set-node-value! n (f (node-value n)))
  (when (branch-node? n)
      (tree-apply f (branch-node-left n))
      (tree-apply f (branch-node-right n))))

(tree-apply (lambda (x) (+ 1 x)) my-tree)
(print-tree my-tree)


(and #t (even? 42) "a")

(define (my-and x . xs)
  (display x)
  (display " | ")
  (displayln xs)
  (if (null? xs)
      x
      (if x
        (apply my-and xs)
        #f)))

(my-and #t (even? 42) "a")

(define-syntax my-and-m
  (syntax-rules ()
    ((_) #t)
    ((_ expr) expr)
    ((_ expr other-exprs ...)
     (if expr
         (my-and-m other-exprs ...)
         #f))))

(my-and-m #t #f (even? 42) "a")


(define-syntax report
  (syntax-rules ()
    ((_ expr)
     (begin
       (display "Report: ")
       (displayln 'expr)
       expr))))

; different arity
(define-syntax defn
  (syntax-rules ()
    ((_ name (params body ...) ...)
     (begin
       (displayln 'name)
       (display "Params: ")(displayln '('params ...))
       (display "Body: ")(displayln '('body ... ...))
       (define (name . arg-v)
         (display "Calling ")(display 'name)(displayln arg-v)
         (let ((arg-v (length arg-v)))
           (cond
             ((= arg-v (length 'params))
              (apply (lambda params body ...) arg-v)) ...)))))))

(defn my-func
  (() (displayln "0 args"))
  ((a) (displayln "1 args"))
  ((a b) (displayln "2 args")))
     