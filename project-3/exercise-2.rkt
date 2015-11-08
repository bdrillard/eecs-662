#lang plai
(require racket/trace)

(define reify-cfwaer
  (lambda (op lhs rhs)
    (if (or (closureV? lhs) (closureV? rhs))
        (error 'interp-cfwaer "arithmetic operand cannot be a function")
        (numV (op (numV-n lhs) (numV-n rhs))))))

(define boxed-CFWAER-value?
  (lambda (v)
    (and (box? v) (CFWAER-Value? (unbox v)))))

(define lookup
  (lambda (name env)
    (type-case Env env
      (mtEnv () (error 'lookup "undefined id"))
      (aSub (bound-name bound-value rest-env)
            (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env)))
      (aRecSub (bound-name box-bound-value rest-env)
               (if (symbol=? bound-name name)
                   (unbox box-bound-value)
                   (lookup name rest-env))))))

(define op-list
  (list (list 'add +)
        (list 'sub -)
        (list 'mul *)
        (list 'div /)))

(define-type CFWAER
  (num (n number?))
  (id (name symbol?))
  (op (proc symbol?))
  (binop (proc op?) (lhs CFWAER?) (rhs CFWAER?))
  (if0 (check CFWAER?) (then-body CFWAER?) (else-body CFWAER?))
  (with (id symbol?) (bound-expr CFWAER?) (bound-body CFWAER?))
  (fun (id symbol?) (body CFWAER?))
  (rec (id symbol?) (expr CFWAER?) (body CFWAER?))
  (app (fun-expr CFWAER?) (expr CFWAER?)))

(define-type CFWAER-Value
  (numV (n number?))
  (closureV (id symbol?) (body CFWAER?) (env Env?)))

(define-type Env
  (mtEnv)
  (aSub (name symbol?) (value CFWAER-Value?) (env Env?))
  (aRecSub (name symbol?) (value boxed-CFWAER-value?) (env Env?)))

(define cyclically-bind-and-interp
  (lambda (bound-id named-expr env)
    (letrec ((value-holder (box (numV 1729)))
             (new-env (aRecSub bound-id value-holder env))
             (named-expr-val (interp-cfwaer named-expr new-env)))
      (begin
        (set-box! value-holder named-expr-val)
        new-env))))

(define interp-cfwaer
  (lambda (e env)
    (type-case CFWAER e
      (num (n) (numV n))
      (op (p) (cadr (assoc p op-list)))
      (binop (p l r) (reify-cfwaer (interp-cfwaer p env)
                                   (interp-cfwaer l env)
                                   (interp-cfwaer r env)))
      (id (i) (lookup i env))
      (with (i v e) (interp-cfwaer (app (fun i e) v) env))
      (if0 (c t e) (if (= 0 (numV-n (interp-cfwaer c env)))
                       (interp-cfwaer t env)
                       (interp-cfwaer e env)))
      (fun (i e) (closureV i e env))
      (rec (i e b) (interp-cfwaer b
                                  (cyclically-bind-and-interp i e env)))
      (app (f e) (let ((fun-val (interp-cfwaer f env)))
                   (if (numV? fun-val)
                       (error 'interp-cfwaer "number cannot be applied to a value")
                       (interp-cfwaer (closureV-body fun-val)
                                      (aSub (closureV-id fun-val)
                                            (interp-cfwaer e env)
                                            (closureV-env fun-val)))))))))

#| TESTS |#
(test (interp-cfwaer (app (fun 'x (binop (op 'add) (num 1) (num 3))) (num 1)) (mtEnv)) (numV 4))
(test (interp-cfwaer (with 'y (num 1) (app (fun 'x (binop (op 'add) (id 'x) (id 'y))) (num 3))) (mtEnv))
                     (numV 4))
(test (interp-cfwaer (with 'y (num 1) 
                           (with 'f (fun 'x (binop (op 'add) (id 'x) (id 'y))) 
                                 (app (id 'f) (num 3)))) (mtEnv))
      (numV 4))
(test (interp-cfwaer (with 'y (num 1) 
                           (with 'f (fun 'x (binop (op 'add) (id 'x) (id 'y)))
                                 (with 'y (num 100)
                                       (app (id 'f) (num 3))))) (mtEnv))
      (numV 4))
(test (interp-cfwaer 
       (rec 'fac 
         (fun 'x (if0 (id 'x) 
                      (num 1)
                      (binop (op 'mul) (id 'x) (app (id 'fac) (binop (op 'sub) (id 'x) (num 1))))))
         (app (id 'fac) (num 3))) (mtEnv))
      (numV 6))
(test (interp-cfwaer 
       (rec 'fac 
         (fun 'x (if0 (id 'x) 
                      (num 1)
                      (binop (op 'mul) (id 'x) (app (id 'fac) (binop (op 'sub) (id 'x) (num 1))))))
         (app (id 'fac) (num 5))) (mtEnv))
      (numV 120))
(test (interp-cfwaer
       (app (fun 'y (app (fun 'x (binop (op 'add) (id 'x) (id 'y))) (num 1))) (num 2))
       (mtEnv))
      (numV 3))
(test (interp-cfwaer
       (rec 'ack
         (fun 'm (fun 'n (if0 (id 'm)
                                        (binop (op 'add) (id 'n) (num 1))
                                        (if0 (id 'n)
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1))) (num 1))
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1)))
                                                  (app (app (id 'ack) (id 'm)) (binop (op 'sub) (id 'n) (num 1))))))))
         (app (app (id 'ack) (num 1)) (num 1)))
       (mtEnv))
      (numV 3))
(test (interp-cfwaer
       (rec 'ack
         (fun 'm (fun 'n (if0 (id 'm)
                                        (binop (op 'add) (id 'n) (num 1))
                                        (if0 (id 'n)
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1))) (num 1))
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1)))
                                                  (app (app (id 'ack) (id 'm)) (binop (op 'sub) (id 'n) (num 1))))))))
         (app (app (id 'ack) (num 2)) (num 2)))
       (mtEnv))
      (numV 7))
(test (interp-cfwaer
       (rec 'ack
         (fun 'm (fun 'n (if0 (id 'm)
                                        (binop (op 'add) (id 'n) (num 1))
                                        (if0 (id 'n)
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1))) (num 1))
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1)))
                                                  (app (app (id 'ack) (id 'm)) (binop (op 'sub) (id 'n) (num 1))))))))
         (app (app (id 'ack) (num 3)) (num 3)))
       (mtEnv))
      (numV 61))
(test (interp-cfwaer
       (rec 'ack
         (fun 'm (fun 'n (if0 (id 'm)
                                        (binop (op 'add) (id 'n) (num 1))
                                        (if0 (id 'n)
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1))) (num 1))
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1)))
                                                  (app (app (id 'ack) (id 'm)) (binop (op 'sub) (id 'n) (num 1))))))))
         (app (app (id 'ack) (num 0)) (num 3)))
       (mtEnv))
      (numV 4))
(test (interp-cfwaer
       (rec 'ack
         (fun 'm (fun 'n (if0 (id 'm)
                                        (binop (op 'add) (id 'n) (num 1))
                                        (if0 (id 'n)
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1))) (num 1))
                                             (app (app (id 'ack) (binop (op 'sub) (id 'm) (num 1)))
                                                  (app (app (id 'ack) (id 'm)) (binop (op 'sub) (id 'n) (num 1))))))))
         (app (app (id 'ack) (num 3)) (num 0)))
       (mtEnv))
      (numV 5))
(test (interp-cfwaer (app (fun 'x (binop (op 'add) (num 1) (num 3))) (num 1)) (mtEnv))
      (numV 4))
(test (interp-cfwaer (rec 'y (num 1) (with 'f (fun 'x (binop (op 'add) (id 'y) (id 'x)))
                                           (app (id 'f) (num 3))))
                     (mtEnv))
      (numV 4))
(test (interp-cfwaer (rec 'y (num 1) (rec 'f (fun 'x (binop (op 'add) (id 'y) (id 'x)))
                                       (with 'y (num 100) (app (id 'f) (num 3)))))
                     (mtEnv))
      (numV 4))
(test (interp-cfwaer (rec 'y (num 1) (rec 'f (fun 'x (binop (op 'add) (id 'y) (id 'x)))
                                       (rec 'y (num 100) (app (id 'f) (num 3)))))
                     (mtEnv))
      (numV 4))