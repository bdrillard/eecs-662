#lang plai

(define reify-cfwaes
  (lambda (op lhs rhs)
    (if (or (closureV? lhs) (closureV? rhs))
        (error 'interp-cfwaer "arithmetic operand cannot be a function")
        (numV (op (numV-n lhs) (numV-n rhs))))))

(define op-list
  (list (list 'add +)
        (list 'sub -)
        (list 'mul *)
        (list 'div /)))

(define next-loc
  (let ((loc (box 0)))
    (lambda ()
      (begin (set-box! loc (+ (unbox loc) 1))
             (unbox loc)))))

(define-type CFWAES
  (num (n number?))
  (id (name symbol?))
  (op (proc symbol?))
  (binop (proc op?) (lhs CFWAES?) (rhs CFWAES?))
  (if0 (check CFWAES?) (then-body CFWAES?) (else-body CFWAES?))
  (with (id symbol?) (bound-expr CFWAES?) (bound-body CFWAES?))
  (fun (id symbol?) (body CFWAES?))
  (app (fun-expr CFWAES?) (body CFWAES?))
  (seq (expr0 CFWAES?) (expr1 CFWAES?))
  (assign (i symbol?) (expr CFWAES?)))

(define-type Env
  (mtEnv)
  (aSub (name symbol?) (loc number?) (rest Env?)))

(define-type Store
  (mtSto)
  (aSto (loc number?) (value CFWAES-Value?) (rest Store?)))

(define-type ValueXStore
  (vxs (value CFWAES-Value?) (store Store?)))

(define-type CFWAES-Value
  (numV (n number?))
  (closureV (id symbol?) (body CFWAES?) (env Env?)))

(define lookup-sto
  (lambda (loc sto total-sto)
    (type-case Store sto
      (mtSto () (error 'lookup-sto "undefined id in store"))
      (aSto (sto-loc sto-val rest-sto)
            (if (equal? sto-loc loc)
                (vxs sto-val total-sto)
                (lookup-sto loc rest-sto total-sto))))))

(define lookup-val
  (lambda (name env sto)
    (type-case Env env
      (mtEnv () (error 'lookup-val "undefined id in environment"))
      (aSub (bound-name bound-value rest-env)
            (if (symbol=? bound-name name)
                (lookup-sto bound-value sto sto)
                (lookup-val name rest-env sto))))))

(define lookup-loc
  (lambda (name env)
    (type-case Env env
      (mtEnv () (error 'lookup-loc "undefined id in location"))
      (aSub (bound-name bound-value rest-env)
            (if (symbol=? bound-name name)
                bound-value
                (lookup-loc name rest-env))))))

(define interp-cfwaes
  (lambda (expr env sto)
    (type-case CFWAES expr
      (num (n) (vxs (numV n) sto))
      (op (p) (cadr (assoc p op-list)))
      (binop (p l r) (let ((op (interp-cfwaes p env sto)))
                       (type-case ValueXStore (interp-cfwaes l env sto)
                         (vxs (l-val l-sto)
                              (type-case ValueXStore (interp-cfwaes r env l-sto)
                                (vxs (r-val r-sto)
                                     (vxs (reify-cfwaes op l-val r-val)
                                          r-sto)))))))
      (id (i) (lookup-val i env sto))
      (fun (i e) (vxs (closureV i e env) sto))
      (app (f a) (type-case ValueXStore (interp-cfwaes f env sto)
                   (vxs (fun-val fun-sto)
                        (if (numV? fun-val)
                            (error 'inerp-cfwaes "number cannot be applied to a value")
                            (type-case ValueXStore (interp-cfwaes a env fun-sto)
                              (vxs (arg-val arg-sto)
                                   (let ((new-loc (next-loc)))
                                     (interp-cfwaes (closureV-body fun-val)
                                                    (aSub (closureV-id fun-val)
                                                          new-loc
                                                          (closureV-env fun-val))
                                                    (aSto new-loc
                                                          arg-val
                                                          arg-sto)))))))))
      (if0 (c t e) (type-case ValueXStore (interp-cfwaes c env sto)
                     (vxs (c-val c-sto)
                          (if (= 0 (numV-n c-val))
                              (interp-cfwaes t env sto)
                              (interp-cfwaes e env sto)))))
      (with (i v e) (interp-cfwaes (app (fun i e) v) env sto))
      (assign (i expr) (type-case ValueXStore (interp-cfwaes expr env sto)
                         (vxs (v s)
                              (vxs v 
                                   (aSto (lookup-loc i env)
                                         v
                                         s)))))
      (seq (e1 e2) (type-case ValueXStore (interp-cfwaes e1 env sto)
                     (vxs (e1-val e1-sto)
                          (interp-cfwaes e2 env e1-sto)))))))

(define eval-cfwaes
  (lambda (expr)
    (vxs-value (interp-cfwaes expr (mtEnv) (mtSto)))))

#| TESTS |#
(test (eval-cfwaes (num 1)) (numV 1))
(test (eval-cfwaes (binop (op 'add) (num 1) (num 2))) (numV 3))
(test (eval-cfwaes (binop (op 'div) (num 4) (num 2))) (numV 2))
(test (eval-cfwaes (with 'x (binop (op 'add) (num 1) (num 3)) (id 'x))) (numV 4))
(test (eval-cfwaes (fun 'x (id 'x))) (closureV 'x (id 'x) (mtEnv)))
(test (eval-cfwaes (app (fun 'x (id 'x)) (num 1))) (numV 1))
(test (eval-cfwaes (app (fun 'x (app (id 'x) (num 3)))
                        (fun 'y (binop (op 'add) (id 'y) (num 1)))))
      (numV 4))
(test (eval-cfwaes (with 'y (num 1)
                         (with 'f (fun 'x (binop (op 'add) (id 'x) (id 'y)))
                               (with 'y (num 3)
                                     (app (id 'f) (num 3))))))
      (numV 4))
(test (eval-cfwaes (binop (op 'add) 
                          (seq (assign 'x (num 1))
                               (app (fun 'y (binop (op 'add) (id 'y) (num 1)))
                                    (id 'x)))
                          (num 1)))
      (numV 3))
(test/exn (eval-cfwaes (app (fun 'x (binop (op 'add) (id 'x) (num 1))) (id 'y)))
          "undefined id")
(test/exn (eval-cfwaes (app (num 1) (num 1))) 
          "number cannot be applied")
(test/exn (eval-cfwaes (binop (op 'mul) (fun 'x (id 'x)) (num 0)))
          "arithmetic operand cannot be a function")

#| EXERCISE TESTS |#
(test (eval-cfwaes (with 'y (num 0)
                         (with 'inc (fun 'x (binop (op 'add) (num 1) (id 'x)))
                               (seq (seq (assign 'y (app (id 'inc) (id 'y)))
                                         (assign 'y (app (id 'inc) (id 'y))))
                                    (seq (assign 'y (app (id 'inc) (id 'y)))
                                         (assign 'y (app (id 'inc) (id 'y))))))))
      (numV 4))
(test (eval-cfwaes (with 'y (num 1)
                         (with 'inc (fun 'x (binop (op 'add) (id 'x) (id 'y)))
                               (app (id 'inc) (num 3)))))
      (numV 4))
(test (eval-cfwaes (with 'y (num 1)
                         (with 'inc (fun 'x (binop (op 'add) (id 'x) (id 'y)))
                               (seq (assign 'y (num 2))
                                    (app (id 'inc) (num 3))))))
      (numV 5))
(test (eval-cfwaes (with 'y (num 1)
                         (with 'inc (seq (assign 'y (num 2))
                                         (fun 'x (binop (op 'add) (id 'x) (id 'y))))
                               (app (id 'inc) (num 3)))))
      (numV 5))
(test (eval-cfwaes (with 'x (num 3)
                         (seq (id 'x)
                              (assign 'x (binop (op 'add) (id 'x) (num 1))))))
      (numV 4))
(test (eval-cfwaes (with 'x (num 3)
                         (seq (assign 'x (binop (op 'add) (id 'x) (num 1)))
                              (assign 'x (binop (op 'add) (id 'x) (num 1))))))
      (numV 5))
(test (eval-cfwaes (with 'x (num 3)
                         (seq (seq (assign 'x (binop (op 'add) (id 'x) (num 1)))
                                   (assign 'x (binop (op 'add) (id 'x) (num 1))))
                              (assign 'x (binop (op 'add) (id 'x) (num 1))))))
      (numV 6))