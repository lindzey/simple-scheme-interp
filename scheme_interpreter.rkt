#lang racket
;; interpreter for Scheme, worked on w/ Lindsey Kuper
;; grammar is
;; v ::= '(num n) | '(func x e)
;; x ::= '(var a)
;; e ::= x | v | '(plus e e) | '(minus e e) | '(times e e) |
;;        '(app e e) 

(require racket/match)

(provide interp)

;; this expects a 2nd argument that's a list of pairs, where the first
;; element in each pair is the var's name, and the 2nd is it's value
(define (interp expr vars)
  (match expr
    [(list 'var x) (interp (lookup-var x vars) vars)]
    [(list 'num n) n]
    ;; f-interp will be 'func, no idea what a is
    [(list 'app f a) (let ((f-interp (interp f vars))
                           (a-interp (interp a vars)))
                       (apply-func f-interp a-interp vars))]
    [(list 'func x e) (list 'func x e)]
    [(list 'plus e1 e2) (+ (interp e1 vars) (interp e2 vars))]
    [(list 'minus e1 e2) (- (interp e1 vars) (interp e2 vars))]
    [(list 'times e1 e2) (* (interp e1 vars) (interp e2 vars))]
    [_ ((error "Invalid expression"))]))

(define (apply-func f a vars)
  (match f
    [(list 'func var expr) (interp expr (add-var vars var a ))]
    [_ ((error "apply-func: Invalid function"))]))

(define (add-var vars x val)
  (append vars (list (cons x val))))

(define (lookup-var x vars)
  (let ((match (assoc x vars)))
    (if (not match)
        (error "unbound variable")
        (cdr match))))


