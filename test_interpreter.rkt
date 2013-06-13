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
   (check-equal? (interp expr1 '()) ans1 "plus only")
   (check-equal? (interp expr2 '()) ans2 "minus only")
   (check-equal? (interp expr3 '()) ans3 "minus/plus nested")
   (check-equal? (interp expr4 '()) ans4 "times only")))

(run-tests op-tests)


(define var-tests
  (test-suite
   "testing simple variable substitution"
   (check-equal? (interp '(plus (num 3) (var a)) '((a . 3) (b . 4))) 6)
   (check-equal? (interp '(plus (num 3) (var b)) '((a . 3) (b . 4))) 7)))

(run-tests var-tests)

(define func-tests
  (test-suite
   "testing the addition of functions as a value"
   (check-equal? (interp '(app (func x (var x)) (num 5)) '()) 5)
   (check-equal? (interp '(app 
                           (app 
                            (func x (func y (plus (var x) (var y)))) 
                            (num 5)) 
                           (num 6)) 
                         '())
                 11)
   ))
(run-tests func-tests)