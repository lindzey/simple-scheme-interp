#lang racket
;; interpreter for Scheme, worked on w/ Lindsey Kuper
;; grammar is
;; v ::= '(num n)
;; x ::= '(var a)
;; e ::= x | v | '(plus e e) | '(minus e e) | '(times e e) 
;; 

(require racket/match)

(provide interp)

(define (interp-first expr)
  (match expr
    [(list 'num n) n]
    [(list 'plus e1 e2) (+ (interp e1) (interp e2))]
    [(list 'minus e1 e2) (- (interp e1) (interp e2))]
    [(list 'times e1 e2) (* (interp e1) (interp e2))]
    [_ ((error "Invalid expression"))]))

;; this expects a 2nd argument that's a list of pairs, where the first
;; element in each pair is the var's name, and the 2nd is it's value
(define (interp expr vars)
  (match expr
    [(list 'var x) (interp (lookup-var x vars) vars)]
    [(list 'num n) n]
    [(list 'plus e1 e2) (+ (interp e1 vars) (interp e2 vars))]
    [(list 'minus e1 e2) (- (interp e1 vars) (interp e2 vars))]
    [(list 'times e1 e2) (* (interp e1 vars) (interp e2 vars))]
    [_ ((error "Invalid expression"))]))

(define (lookup-var x vars)
  (let ((match (assoc x vars)))
    (if (not match)
        (error "unbound variable")
        (cdr match))))