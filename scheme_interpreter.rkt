#lang racket
;; interpreter for Scheme, worked on w/ Lindsey Kuper
;; grammar is
;; v ::= '(num n)
;; e ::= v | '(plus e e) | '(minus e e) | '(times e e)

(require racket/match)

(provide interp)

(define (interp expr)
  (match expr
    [(list 'num n) n]
    [(list 'plus e1 e2) (+ (interp e1) (interp e2))]
    [(list 'minus e1 e2) (- (interp e1) (interp e2))]
    [(list 'times e1 e2) (* (interp e1) (interp e2))]
    [_ ((error "Invalid expression"))]))

