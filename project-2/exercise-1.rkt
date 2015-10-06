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

(define op-list
  (list (list 'add +)
        (list 'sub -)
        (list 'mul *)
        (list 'div /)))

(define-type CFAE
  (num (n number?))
  (id (name symbol?))
  (op (proc symbol?))
  (binop (proc op?) (lhs CFAE?) (rhs CFAE?))
  (if0 (check CFAE?) (then-body CFAE?) (else-body CFAE?))
  (fun (id symbol?) (body CFAE?))
  (app (fun-expr CFAE?) (expr CFAE?)))

(define-type DefrdSub
  (mtSub)
  (aSub (name symbol?) (value CFAE?) (ds DefrdSub?)))

(define interp-cfae
  (lambda (e ds)
    (type-case CFAE e
      (num (n) (num n))
      (op (p) (cadr (assoc p op-list)))
      (binop (p l r) (reify-cfae (interp-cfae p ds) (interp-cfae r ds) (interp-cfae l ds)))
      (id (i) (lookup i ds))
      (fun (i e) (fun i e))
      (app (f e) (let ((fun-val (interp-cfae f ds)))
                   (if (num? fun-val)
                       (error 'interp-cfae "number cannot be applied to a value")
                       (interp-cfae (fun-body fun-val)
                                    (aSub (fun-id fun-val) (interp-cfae e ds) ds)))))
      (if0 (c t e) (if (equal? (num 0) (interp-cfae c ds))
                       (interp-cfae t ds)
                       (interp-cfae e ds))))))

#| TESTS |#
(test (interp-cfae (num 1) (mtSub)) (num 1))
(test (interp-cfae (binop (op 'add) (num 1) (num 1)) (mtSub)) (num 2))
(test (interp-cfae (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (num 1)) (mtSub))
      (num 2))
(test (interp-cfae (app (fun 'x (app (id 'x) (num 3))) 
                        (fun 'y (binop (op 'add) (id 'y) (num 1)))) (mtSub)) 
      (num 4))
(test (interp-cfae (if0 (num 1) (num 1) (num 2)) (mtSub)) (num 2))
(test (interp-cfae (if0 (app (fun 'x (binop (op 'sub) (id 'x) (num 3))) (num 3))
                        (num 0)
                        (num 1))
                   (mtSub))
      (num 0))
(test/exn (interp-cfae (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (id 'y)) (mtSub))
          "undefined id")
(test/exn (interp-cfae (app (num 1) (num 1)) (mtSub)) 
          "number cannot be applied")
(test/exn (interp-cfae (binop (op 'mul) (fun 'x (id 'x)) (num 0)) (mtSub))
          "arithmetic operand cannot be a function")