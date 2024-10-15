#lang racket

(define a 'apple)

(set! a 'banana) ; To change the variable value!

a

(define x 5)

(define (change-value x)
  (set! x 2)
  (displayln x))

(change-value x)

x ; x will not be overwritten, because in raket we pass by value

(define v (vector 1 2 3)) ; we can't use #(1 2 3) because this way of definition is immutable

(define (change-vector v)
  (vector-set! v 1 0) ; change item '2' to '0'
  (displayln v))

(change-vector v) ; Values are changing because we are passing a reference!

v

(define k 10)

(define (f)
  k)

(define (g)
  (define k 5)
  (f))

(g) ; Returns '10' because Statically scoped

(define ada "Ada")
ada

(let ((bob "Bob"))
  (displayln ada)
  (displayln bob))
; We cannot call 'bob' since it's not in the scope
; bob => not defined


; Named let
(define (loop-ten-times)
  (let loop ((i 0))
    (when (< i 10)
      (displayln (string-append "Loop " (~a (+ i 1)) " times"))
      (loop (+ i 1)))))
(loop-ten-times)

(define (loop-ten-times-alt)
  (let ((i 0))
    (let loop ()
      (when (< i 10)
        (displayln (string-append "Loop-alt " (~a (+ i 1)) " times"))
        (set! i (+ i 1))
        (loop)))))
(loop-ten-times-alt)

(define (split-sum x . xs)
  (displayln x)
  (displayln xs)
  (+ x
     (apply + xs)))
; xs is a list, for example, '(2 3 4 5)
; We use 'apply' because 'x' cannont be used for a list in a form '(1 2 3)
(split-sum 1 2 3 4 5)

(define (list-flatten lst)
  (cond ((null? lst) lst)
        ((not (list? lst)) (list lst))
        (else (append (list-flatten (car lst))(list-flatten (cdr lst))))))

(list-flatten '(1 (2 (3 4) 5 6) (7 8)))


;; STRUCTURES

(struct person
  (name
   (age #:mutable)))

(define p1 (person "Ada" 25))
(define p2 "Bob")

(person? p1)
(person? p2)

(set-person-age! p1 26)
(person-age p1)


(struct node
  ((value #:mutable)))

(struct binary-node node
  (left
   right))

(define (leaf? n)
  (and (node? n) (not (binary-node? n))))

(define a-tree (binary-node 2 (binary-node 3 (node 4) (node 2)) (node 1)))

(define (print-tree n)
  (displayln (node-value n))
  (unless (leaf? n)
    (print-tree (binary-node-left n))
    (print-tree (binary-node-right n))))

(print-tree a-tree)

; Apply is a higher-order function for trees
; applies a function f(x) to the value x of each tree node

(define (tree-apply f n)
  (set-node-value! n (f (node-value n)))
  (unless (leaf? n)
    (begin
      (tree-apply f (binary-node-left n))
      (tree-apply f (binary-node-right n)))))

(tree-apply add1 a-tree)
(println "---")
(print-tree a-tree)

(tree-apply (lambda (x) (+ 5 x)) a-tree)
(println "---")
(print-tree a-tree)

; Closure
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ 1 count))
      count)))

(define counter1 (make-counter)) ; Two independent counters
(define counter2 (make-counter))

(displayln (counter1))
(displayln (counter1))
(displayln (counter2))
(displayln (counter1))
(displayln (counter2))

; MACROS

(define (say-hello . people) ; to pass a list
  (displayln (string-append "Hello " (string-join people))))

(say-hello "Ada" "Bob" "Carl")

; In a form of a macro

(define-syntax hello
  (syntax-rules () ; In this list we put the literals that should not be binded (decorative ones)
    ((_ names ...) ; 'names ...' matches a sequence of items
     (displayln (string-append "Hello " (string-join (list names ...)))))))

(hello "Lucus" "Uca" "Bibi Lu")

(define-syntax while
  (syntax-rules (do) ; In this list we put the literals that should not be binded (decorative ones)
    ((_ cond do body ...)
     (let loop ()
       (when cond
         (begin
           body ...
           (loop)))))))

(displayln "while-do loop")
(define i 0)
(while (< i 5) do
       (set! i (+ 1 i))
       (displayln i))

; (for x in <list> <body>)

(define-syntax for
  (syntax-rules (in)
    ((_ item in lst body ...)
     (begin
       (unless (list? lst)
         (error "Not a list"))
       (let loop ((item (car lst))
                  (rest (cdr lst)))
         (begin
           body ...
           (unless (null? rest)
             (loop (car rest)(cdr rest)))))))))

(displayln "for-in loop")
(for x in '(1 2 3 4 5) (displayln x))
;; (for i in "pizza" (displayln x))

; RECURSIVE MACROS
; you can call the macro itself after you define it.
; useful if you have multiple syntax rules that match different conditions

(define-syntax say
  (syntax-rules (hello goodbye)
    ((_ hello) (displayln "hello"))
    ((_ goodbye) (displayln "goodbye"))
    ((_ ...) (displayln "whatever...")))) ; catch all case

(say hello)
(say goodbye)
(say ciao)