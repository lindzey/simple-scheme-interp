#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "scheme_interpreter.rkt")

(define expr1 '(plus (num 3) (num 4)))
(define ans1 7)
(define expr2 '(minus (num 6) (num 1)))
(define ans2 5)
(define expr3 '(minus (plus (num 6) (num 5))
                      (num 2)))
(define ans3 9)
(define expr4 '(times (num 2) (num 4)))
(define ans4 8)

(define op-tests
  (test-suite 
   "tests of plus, minus, times"
   (check-equal? (interp expr1) ans1 "plus only")
   (check-equal? (interp expr2) ans2 "minus only")
   (check-equal? (interp expr3) ans3 "minus/plus nested")
   (check-equal? (interp expr4) ans4 "times only")))

(run-tests op-tests)