#lang racket

(displayln "Hello World!")

(define x 5)

(define (say-hello)
  (displayln "Hello World from the function!"))

(say-hello)

(define (greet name)
  (if (string? name)
      (displayln (string-append "Hello " name "!"))
      (displayln "Input is not a string!")))

(greet 3)

(define (greet-only-luca name)
  (if (eq? name "Luca")
      (displayln (string-append "Hello " name "!"))
      (displayln "You are not Luca :(((")))

(greet-only-luca "Yulya")

; Bad practice! We can run out of memory

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

; Tail recursion!!!

(define (factorial-tail-rec n acc)
  (if (zero? n)
      acc
      (factorial-tail-rec (- n 1) (* acc n))))

(factorial-tail-rec 6 1)

; Just a label

(define (factorial-2 n)
  (let factorial-loop ((curr n)
                       (acc 1))
    (if (zero? curr)
        acc
        (factorial-loop (- curr 1)(* acc curr)))))

(factorial-2 5)

(define (divisible-by? n m)
  (zero? (modulo n m)))

(define (fizzbuzz n)
  (cond ((divisible-by? n 15) "FizzBuzz")
        ((divisible-by? n 5) "Buzz")
        ((divisible-by? n 3) "Fizz")
        (else n)))

(define numbers-0 (range 1 31)) ; Like in Python

(map fizzbuzz numbers-0)

(define numbers-1 (list 1 2 3 4 5))

numbers-1

(define numbers '(1 2 3 4 5))

numbers

(define quasi-numbers `(1 2 ,(+ 1 4) 4 5)) ; Backtick, unquoting

quasi-numbers

(map (lambda (x)(+ x 1)) numbers)

(apply + numbers)

numbers

(foldl + 0 numbers)

(foldl * 1 numbers) ; Factorial

(foldl * 1 (range 1 10))

(define z 3)

(displayln (string-append "z = " (~a z))) ; ~a to convert number to string

(cons 2 (cons 1 '()))

(cons 1 2) ; NOT a proper list

(define (reverse-list lst)
  (define (reverse-list-helper x acc)
    (displayln (string-append "x = " (~a x) "; acc = " (~a acc)))
    (if (null? x)
        acc
        (reverse-list-helper(cdr x)
                            (cons (car x) acc))))
  (reverse-list-helper lst '()))

(reverse-list numbers)

(empty? '())
(pair? '())
(pair? '(1))

