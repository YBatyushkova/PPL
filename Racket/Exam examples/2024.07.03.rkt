#lang racket

(define-syntax let-cond+
  (syntax-rules ()
    ((_ ((condition bindings then-part)...) else-part)
     (let ((x null))
       (when condition
         (set! x (let bindings then-part)))
       ...
       (if (null? x)
           else-part
           x)))))

(define-syntax let-cond++
  (syntax-rules ()
    ((_ ((condition bindings then-part)...) else-part)
     (let ((flag #f)
           (result #f))
       (when condition
         (set! result
               (let bindings
                 (set! flag #t)
                 then-part)))
       ...
       (if flag
           result
           else-part)))))

(let-cond+
 (((< 5 13)
   ((a 10))
   (displayln "hello"))
  ((= 5 5)
   ((b 3)
    (c 4))
   (displayln "ciao")))
 "all conditions are false")
   