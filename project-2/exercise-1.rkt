#lang plai
(require racket/trace)

(define lookup
  (lambda (name ds)
    (type-case DefrdSub ds
      (mtSub () (error 'lookup "undefined id"))
      (aSub (bound-name bound-value rest-ds)
            (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-ds))))))

(define-type CFAE
  (num (n number?))
  (id (name symbol?))
  (add (lhs CFAE?) (rhs CFAE?)) ; Can we just use binop again? 
  (sub (lhs CFAE?) (rhs CFAE?))
  (mul (lhs CFAE?) (rhs CFAE?))
  (div (lhs CFAE?) (rhs CFAE?))
  (if0 (check CFAE?) (else-body CFAE?) (then-body CFAE?))
  (fun (id symbol?) (body CFAE?))
  (app (fun-expr CFAE?) (expr FWAE?)))

(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?) (value CFAE?) (ds DefrdSub?)))

(define interp-cfae
  (lambda (expr ds)
    (num (n) (num n)) ; Said interp should return AS, how do we compute values then? 
    (add (l r) (+ (interp-cfae l ds) (interp-cfae r ds)))
    (sub (l r) (- (interp-cfae l ds) (interp-cfae r ds)))
    (mul (l r) (* (interp-cfae l ds) (interp-cfae r ds)))
    (div (l r) (/ (interp-cfae l ds) (interp-cfae r ds)))
    (fun (i e) ())
    (id (n) (lookup n ds))))

