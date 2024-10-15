#lang racket

; COND

(define (split_in_ranges_helper lst x y before between after)
  (if (empty? lst)
      (list before between after)
      (let ((a (car lst)))
        (cond
          ((and (< a x) (< a y)) (split_in_ranges_helper (cdr lst) x y (cons a before) between after))
          ((and (>= a x) (<= a y)) (split_in_ranges_helper (cdr lst) x y before (cons a between) after))
          (else (split_in_ranges_helper (cdr lst) x y before between (cons a after)))))))


(define (split_in_ranges lst x y)
  (split_in_ranges_helper lst x y '() '() '()))

(split_in_ranges '(1 2 3 4 8 4 5 2 36 7) 4 7)