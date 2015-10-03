#lang plai
(require racket/trace)

(define reify-cfae
  (lambda (op lhs rhs)
    (if (or (fun? lhs) (fun? rhs))
        (error 'interp-cfae "arithmetic operand cannot be a function")
        (num (op (num-n lhs) (num-n rhs))))))

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
  (add (lhs CFAE?) (rhs CFAE?))
  (sub (lhs CFAE?) (rhs CFAE?))
  (mul (lhs CFAE?) (rhs CFAE?))
  (div (lhs CFAE?) (rhs CFAE?))
  (if0 (check CFAE?) (then-body CFAE?) (else-body CFAE?))
  (fun (id symbol?) (body CFAE?)) ; functions are single argument? 
  (app (fun-expr CFAE?) (expr CFAE?))) ; only applying lambdas?, nested lambdas?, multivariable expressions are nested lambdas

(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?) (value CFAE?) (ds DefrdSub?)))

(define interp-cfae
  (lambda (e ds)
    (type-case CFAE e
      (num (n) (num n))
      (add (l r) (reify-cfae + (interp-cfae l ds) (interp-cfae r ds)))
      (sub (l r) (reify-cfae - (interp-cfae l ds) (interp-cfae r ds)))
      (mul (l r) (reify-cfae * (interp-cfae l ds) (interp-cfae r ds)))
      (div (l r) (reify-cfae / (interp-cfae l ds) (interp-cfae r ds)))
      (id (i) (lookup i ds))
      (fun (i e) (fun i e))
      (app (f e) (let ((fun-val (interp-cfae f ds)))
                   (if (num? fun-val)
                       (error 'interp-cfae "number cannot be applied to a value")
                       (interp-cfae (fun-body fun-val)
                                    (aSub (fun-id fun-val) (interp-cfae e ds) ds)))))
      (if0 (c t e) (if (= 0 (num-n (interp-cfae c ds)))
                       (interp-cfae t ds)
                       (interp-cfae e ds))))))