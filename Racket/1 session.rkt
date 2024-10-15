#lang racket

(define (hello)
 (displayln "hello world"))


(define (len L) ;; length of the list L
  (define (tail-len L k)
    (if (null? L) k
        (tail-len (cdr L) (+ k 1))))
  (tail-len L 0))

(len '(1 3 4))
(length '(1 3 4))


(define (prefix n L) ;; n first elements of the list L
  (if (= n 0)
      '()
      (cons (car L)
            (prefix (- n 1) (cdr L)))))

(prefix 3 '(1 2 5 6 7 9 1))
(take '(1 2 5 6 7 9 1) 3)


(define (ref k L)
  (if (= k 0)
      (car L)
      (ref (- k 1) (cdr L))))

(ref 1 '(3 7 2)) ;; returns i element of a list
(vector-ref #(3 7 2) 1) ;; returns i element of a vector


; (range 3) (0 1 2 3) like in Python
; (range 2 3) (2 3)

;(define (range s e) ;; recurrent variant
;  (if (= s e)
;      (list s)
;      (cons s (range (+ s 1) e))))

;(range 2 4)

(define (range s . e)
  (define (r s e)
    (if (= s e)
        (list s)
        (cons s (r (+ s 1) e))))
  (if (null? e)
      (r 0 s)
      (r s (car e))))

(range 2 5)


; THUNK - to avoid evoluation
; (lambda () (+ 1 2))


(define (test-while)
  (define x 0)
  (while (lambda()
           (< x 10))
         (lambda ()
           (displayln x)
           (set! x (+ x 1)))))

(define (while c b)
  (when (c)
    (b)
    (while c b)))