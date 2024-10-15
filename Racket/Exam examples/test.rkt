#lang racket

(define cont-queue '())

(define (for-each/cc cond lst body)
  (for-each (lambda(x)
              (body x)
              (when (cond x)
                (call/cc (lambda(m)
                           (set! cont-queue (append cont-queue (list m)))))))
            lst))

(newline)

(for-each/cc odd?
             '(1 2 3 4)
             (lambda (x) (displayln x)))

(define (use-cc)
  (unless (empty? cont-queue)
    (let ((c (car cont-queue)))
      (set! cont-queue (cdr cont-queue))
      (c))))

(newline)
(use-cc)