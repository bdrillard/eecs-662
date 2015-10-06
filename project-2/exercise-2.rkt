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

(define-type CFWAE
  (wnum (n number?))
  (wid (name symbol?))
  (wop (proc symbol?))
  (wbinop (proc wop?) (lhs CFWAE?) (rhs CFWAE?))
  (wwith (id symbol?) (bound-expr CFWAE?) (bound-body CFWAE?))
  (wif0 (check CFWAE?) (then-body CFWAE?) (else-body CFWAE?))
  (wcond0 (bindings CondBind?))
  (wfun (id symbol?) (body CFWAE?))
  (wapp (fun-expr CFWAE?) (expr CFWAE?)))

(define-type CondBind
  (welse (expr CFWAE?))
  (wbind (test CFWAE?) (then0 CFWAE?) (rest CondBind?)))

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

(define elab
  (lambda (e)
    (type-case CFWAE e
      (wnum (n) (num n))
      (wid (i) (id i))
      (wop (p) (op p))
      (wbinop (p l r) (binop (elab p) (elab l) (elab r)))
      (wwith (i v e) (app (fun i (elab e)) (elab v)))
      (wif0 (c t e) (if0 (elab c) (elab t) (elab e)))
      (wcond0 (bs) (let loop ((bind bs))
                     (type-case CondBind bind
                       (welse (e) (elab e))
                       (wbind (c t r) (if0 (elab c)
                                           (elab t)
                                           (loop r))))))
      (wfun (i e) (fun i (elab e)))
      (wapp (f e) (app (elab f) (elab e))))))

(define interp-cfae
  (lambda (e ds)
    (type-case CFAE e
      (num (n) (num n))
      (op (p) (cadr (assoc p op-list)))
      (binop (p l r) (reify-cfae (interp-cfae p ds) (interp-cfae l ds) (interp-cfae r ds)))
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

(define prelude
  (aSub 'pi (num 3.14159)
  (aSub 'square (fun 'x (binop (op 'mul) (id 'x) (id 'x)))
  (aSub 'area (fun 'r (binop (op 'mul) (id 'pi) (app (id 'square) (id 'r))))
  (aSub 'inc (fun 'x (binop (op 'add) (id 'x) (num 1))) (mtSub))))))

(define eval-cfwae
  (lambda (e)
    (interp-cfae (elab e) prelude)))

#| TESTS |#
(test (eval-cfwae (wnum 1)) (num 1))
(test (eval-cfwae (wbinop (wop 'add) (wnum 1) (wnum 1))) (num 2))
(test (eval-cfwae (wapp (wfun 'x (wbinop (wop 'add) (wid 'x) (wnum 1))) (wnum 1))) 
      (num 2))
(test (eval-cfwae (wapp (wfun 'x (wapp (wid 'x) (wnum 3)))
                        (wfun 'y (wbinop (wop 'add) (wid 'y) (wnum 1)))))
      (num 4))
(test (eval-cfwae (wwith 'x (wfun 'y (wbinop (wop 'div) (wid 'y) (wnum 2)))
                         (wapp (wid 'x) (wnum 2))))
      (num 1))
(test (eval-cfwae (wwith 'n (wnum 10)
                         (wwith 'f (wfun 'x (wbinop (wop 'add) (wid 'x) (wid 'n)))
                                (wwith 'n (wnum 5)
                                       (wapp (wid 'f) (wnum 10))))))
      (num 15))
(test (eval-cfwae (wif0 (wapp (wfun 'x (wbinop (wop 'sub) (wid 'x) (wnum 3))) (wnum 3))
                       (wnum 0)
                       (wnum 1)))
      (num 0))
(test (eval-cfwae (wcond0 (wbind (wbinop (wop 'sub) (wnum 1) (wnum 1)) (wnum 1) (welse (wnum 2)))))
      (num 1))
(test (eval-cfwae (wcond0 (wbind (wnum 1) (wnum 1) 
                                 (welse (wif0 (wnum 0) (wnum 0) (wnum 1))))))
      (num 0))
(test (eval-cfwae (wcond0 (welse (wnum 0))))
      (num 0))
(test (eval-cfwae (wapp (wid 'inc) (wnum 0)))
      (num 1))
(test (eval-cfwae (wapp (wid 'area) (wnum 1)))
      (num 3.14159))
(test/exn (eval-cfwae (wapp (wfun 'x (wbinop (wop 'add) (wid 'x) (wnum 1))) (wid 'y)))
          "undefined id")
(test/exn (eval-cfwae (wapp (wnum 1) (wnum 1)))
          "number cannot be applied")
(test/exn (eval-cfwae (wbinop (wop 'mul) (wfun 'x (wid 'x)) (wnum 0)))
          "arithmetic operand cannot be a function")