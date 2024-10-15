#lang racket

(define (iter-vector vec)
  (let ((cur 0) (top (vector-length vec)))
    (lambda ()
      (if (= cur top)
          '<end>
          (let ((v (vector-ref vec cur)))
            (set! cur (+ 1 cur))
            v)))))
          
(define i (iter-vector #(1 2 3)))

(filter (lambda (x) (> x 0)) '(0 10 -11))

(map (lambda (x) (+ 1 x)) '(0 10 -11))

(foldl string-append "+" '("one" " " "two" " " "three"))
(foldr string-append "+" '("one" " " "two" " " "three"))

(foldl * 1 '(1 2 3 4 5 6))


(define-syntax while
  (syntax-rules ()
    ((_ condition body ...)
     (let loop ()
       (when condition
         (begin
           body ...
           (loop)))))))

(define (fact-while x)
  (let ((acc 1) (n x))
    (while (> n 0)
           (set! acc (* acc n))
           (set! n (- n 1)))
    acc))

(define-syntax my-let
  (syntax-rules ()
    ((_ ((var expr) ...) body ...)
     ((lambda(var ...) body ...) expr ...))))

(member 3 '(1 1 2 2 3 4 5))

;;2022.01
(define-syntax block
  (syntax-rules (then where <-)
    ((_ (body-1 ...) then (body-2 ...) where (var <- val-1 val-2) ...)
     (begin
       (let ((var val-1) ...)
         body-1 ...)
       (let ((var val-2) ...)
         body-2 ...)))))

(block
 ((displayln (+ x y))
 (displayln (* x y))
 (displayln (* z z)))
 then
 ((displayln (+ x y))
 (displayln (* z x)))
 where (x <- 12 3)(y <- 8 7)(z <- 3 2))


;;2023.09
(define-syntax multifun
  (syntax-rules ()
    ((_ (func ...) (param ...) ((body ...) ...))
     (begin
       (define (func param ...)
         (body ...))
       ...))))

(multifun (f g) (x y)
 ((+ x x x y)
 (* x x y)))

(f 2 5)
(g 2 5)


;;2020.06
(define-syntax define-with-types
  (syntax-rules (:)
    ((_ (procedure : proc-pred (var : var-pred)...) body ...)

       (define (procedure var ...)
         (if (and (var-pred var)...)
             (let ((res (begin
                          body ...)))
               (if (not (proc-pred res))
                   (error "wrong result type" res)
                   res))
             (error "wrong variable type"))))))        


(define-with-types (add-to-char : integer? (x : integer?) (y : char?))
  (+ x (char->integer y)))

(add-to-char 5 #\q)


;2021.01
(define (multi-list->vector lst)
  (cond
    ((not (list? lst)) lst)
    ((null? (filter list? lst)) (apply vector lst))
    (else (apply vector (map multi-list->vector lst)))))


(multi-list->vector '(1 2 (3 4) (5 (6)) "hi" ((3) 4)))


;;2019.07
(define (split L v1 v2)
  (define (helper L v1 v2 l1 l2 l3)
    (if (null? L)
        (list l1 l2 l3)
        (let ((x (car L))
              (xs (cdr L)))
          (cond
            ((and (< x v1) (< x v2))
             (helper xs v1 v2 (cons x l1) l2 l3))
            ((and (> x v1) (> x v2))
             (helper xs v1 v2 l1 l2 (cons x l3)))
            ((and (>= x v1) (<= x v2))
             (helper xs v1 v2 l1 (cons x l2) l3))))))
  (helper L v1 v2 '() '() '()))

(split '(3 4 7 1 3 5 7 8 9 11 43 22 1 0 4) 7 13)

;;2020.02
(define-syntax each
  (syntax-rules(in until :)
    ((_ x in lst until pred : body ...)
     (let loop ((x (car lst))
                (xs (cdr lst)))
       (unless (null? x)
         (unless pred
           (begin
             body ...
             (loop (car xs) (cdr xs)))))))))


(define-syntax each-until
  (syntax-rules(in until :)
    ((_ x in lst until pred : body ...)
     (let loop ((xs lst))
       (unless (null? xs)
         (let ((x (car xs)))
           (unless pred
             (begin
               body ...
               (loop (cdr xs))))))))))
           
(each x in '(1 2 3 4)
            until (> x 3) :
            (display (* x 3))
            (display " "))
;;2022.06
(define (list-to-compose L)
  (lambda (x)
    (foldr (lambda (f acc) (f acc)) x L)))

;; acc is necessary because we need something to apply f to


;;2023.06
(define (fold-left-right f i l)
  (let loop ((left i)
             (right (lambda(x) x))
             (xs l))
    (if (null? xs)
      (cons left (right i))
      (loop (f (car xs) left)
            (lambda(x)
              (right (f (car xs) x)))
            (cdr xs)))))

(fold-left-right string-append "" '("a" "b" "c"))


;;2020.07
(define-syntax cobol-fold
  (syntax-rules (direction -> <- from data exec using)
    ((_ direction <- from acc data lst ... (exec body ...) using x y)
     (foldl (lambda (x y) (begin body ...)) acc (list lst ...)))
    ((_ direction -> from acc data lst ... (exec body ...) using x y)
     (foldr (lambda (x y) (begin body ...)) acc (list lst ...)))))
    
(displayln "")
(cobol-fold direction <- from 1 data 1 2 3 4 5 6
            (exec
             (displayln y)
             (+ x y))
            using x y)  


;;2021.07
(define-syntax defun
  (syntax-rules ()
    ((_ f (x ...) body ...)
     (define (f x ...)
       body ...))))

(defun qqq (x1 x2) (+ x1 x2))


;;2021.06
(define (mix f . lst)
  (foldr (lambda (x y) (list x y x)) (map f lst) lst))
  
(mix (lambda(x) (+ x 1)) 1 2 3)


;;similar example

(define (combine f . lsts)
  (let ((result (apply map f lsts)))  ;; Apply `f` to corresponding elements of the lists
    (foldr (lambda (x acc) (list (car x) acc (car x))) result lsts)))  ;; Nest the results similarly to `mix`

(combine + '(1 2 3) '(4 5 6) '(7 8 9))

;;output '(1 (4 (7 (12 15 18) 7) 4) 1)

;;2021.02
(define (depth-encode lst)
  (define (helper-enc L)
    (cond
      ((null? L) L)
      ((list?(car L))
       (append (map (lambda (n) (cons (+ 1 (car n)) (cdr n)))
                    (helper-enc (car L)))
               (helper-enc (cdr L))))
      (else (cons (cons 0 (car L))(helper-enc (cdr L))))))
  (helper-enc lst))
          
(depth-encode '(1 (2 3) 4 (((5) 6 (7)) 8) 9 (((10)))))

;;2024.06
(define-syntax let-cond
  (syntax-rules ()
    ((_ [(condition bindings then-body) ...] else-body)
     (cond
       (condition
        (let bindings
          then-body))
       ...
       (else else-body)))))

(let-cond [((> 5 13) [(a 10) (b 20)] (+ a b))
           ((= 5 5) [(c 3) (d 4)] (+ c d))] "all conditions false")

;;2023.01
(define *cont-store* '())

(define (use-scc)
  (when (cons? *cont-store*)
    (let ((c (car *cont-store*)))
      (set! *cont-store* (cdr *cont-store*))
      (c))))

(define (for-each/cc/func cond lst body)
  (when (cons? lst)
    (let ((x (car lst)))
      (call/cc (lambda (c)
                 (when (cond x)
                   (set! *cont-store* (append *cont-store* (list c))))
                 (body x)))
      (for-each/cc cond (cdr lst) body))))


(define-syntax for-each/cc
  (syntax-rules ()
    ((_ cond lst body)
     (for-each (lambda(x)
                 (call/cc (lambda(c)
                            (when (cond x)
                              (set! *cont-store* (append *cont-store* (list c))))
                            (body x))))
               lst))))

(define (use-cc)
  (unless (null? *cont-store*)
    (let ((x (car *cont-store*)))
      (set! *cont-store* (cdr *cont-store*))
      (x))))
    

(for-each/cc odd?
             '(1 2 3 4)
             (lambda (x) (displayln x)))