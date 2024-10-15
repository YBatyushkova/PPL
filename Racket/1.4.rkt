#lang racket

(displayln "Hello world!")

(define x 5)

;function
(define (say-hello)
  (displayln "Hello world!"))

(say-hello)

(define (greet name)
  (if (string? name)
      (displayln(string-append "Hello " name "!"))
      (displayln "Not a string!")))

(greet 3)

;control flow
(define (greet-only-me name)
  (if (equal? name "Yuliya")
   (displayln(string-append "Hello " name "!")) ;then branch
   (displayln "You are not welcome!!!"))) ;else branch

(greet-only-me "Yuliya")
(greet-only-me "qqq")


;factorial function, example of the loop
(define (factorial n)
  (if (zero? n) ;(= 0 n)
      1
      (* n (factorial (- n 1)))))

(factorial 5)