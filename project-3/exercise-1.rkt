#lang plai
(require racket/trace)

(define reify-cfwae
  (lambda (op lhs rhs)
    (if (or (closureV? lhs) (closureV? rhs))
        (error 'interp-cfwae "arithmetic operand cannot be a function")
        (numV (op (numV-n lhs) (numV-n rhs))))))

(define lookup
  (lambda (name env)
    (type-case DefrdSub env
      (mtEnv () (error 'lookup "undefined id"))
      (aSub (bound-name bound-value rest-env)
            (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-env))))))

(define op-list
  (list (list 'add +)
        (list 'sub -)
        (list 'mul *)
        (list 'div /)))

(define-type CFWAE
  (num (n number?))
  (id (name symbol?))
  (op (proc symbol?))
  (binop (proc op?) (lhs CFWAE?) (rhs CFWAE?))
  (if0 (check CFWAE?) (then-body CFWAE?) (else-body CFWAE?))
  (fun (id symbol?) (body CFWAE?))
  (with (id symbol?) (bound-expr CFWAE?) (bound-body CFWAE?))
  (app (fun-expr CFWAE?) (expr CFWAE?)))

(define-type CFWAE-Value
  (numV (n number?))
  (closureV (id symbol?) (body CFWAE?) (env DefrdSub?)))

(define-type DefrdSub
  (mtEnv)
  (aSub (name symbol?) (value CFWAE-Value?) (env DefrdSub?)))

(define interp-cfwae
  (lambda (e env)
    (type-case CFWAE e
      (num (n) (numV n))
      (op (p) (cadr (assoc p op-list)))
      (binop (p l r) (reify-cfwae (interp-cfwae p env)
                                  (interp-cfwae l env)
                                  (interp-cfwae r env)))
      (id (i) (lookup i env))
      (fun (i e) (closureV i e env))
      (with (i v e) (interp-cfwae (app (fun i e) v) env))
      (app (f e) (let ((fun-val (interp-cfwae f env)))
                   (if (numV? fun-val)
                       (error 'interp-cfwae "number cannot be applied to a value")
                       (interp-cfwae (closureV-body fun-val)
                                    (aSub (closureV-id fun-val)
                                          (interp-cfwae e env)
                                          (closureV-env fun-val))))))
      (if0 (c t e) (if (= 0 (numV-n (interp-cfwae c env)))
                       (interp-cfwae t env)
                       (interp-cfwae e env))))))

#| TESTS |#
(test (interp-cfwae (num 1) (mtEnv)) (numV 1))
(test (interp-cfwae (binop (op 'add) (num 1) (num 1)) (mtEnv)) (numV 2))
(test (interp-cfwae (with 'x (num 4) (binop (op 'add) (id 'x) (num 1))) (mtEnv))
      (numV 5))
(test (interp-cfwae (with 'inc (fun 'x (binop (op 'add) (id 'x) (num 1)))
                          (app (id 'inc) (num 1))) (mtEnv))
      (numV 2))
(test (interp-cfwae (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (num 1)) (mtEnv))
      (numV 2))
(test (interp-cfwae (app (fun 'x (app (id 'x) (num 3))) 
                        (fun 'y (binop (op 'add) (id 'y) (num 1)))) (mtEnv)) 
      (numV 4))
(test (interp-cfwae (with 'y (num 1) 
                           (with 'f (fun 'x (binop (op 'add) (id 'x) (id 'y)))
                                 (with 'y (num 2)
                                       (app (id 'f) (num 3))))) (mtEnv))
      (numV 4))
(test (interp-cfwae (if0 (num 1) (num 1) (num 2)) (mtEnv)) (numV 2))
(test (interp-cfwae (if0 (app (fun 'x (binop (op 'sub) (id 'x) (num 3))) (num 3))
                        (num 0)
                        (num 1))
                   (mtEnv))
      (numV 0))
(test/exn (interp-cfwae (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (id 'y)) (mtEnv))
          "undefined id")
(test/exn (interp-cfwae (app (num 1) (num 1)) (mtEnv)) 
          "number cannot be applied")
(test/exn (interp-cfwae (binop (op 'mul) (fun 'x (id 'x)) (num 0)) (mtEnv))
          "arithmetic operand cannot be a function")