#lang plai
(require racket/trace)

(define op-table
  (list (list 'add +)
        (list 'sub -)
        (list 'mult *)
        (list 'div /)))

(define bindings-list?
  (lambda (l)
    (and (list? l) (andmap binding? l))))

(define bindable-body?
  (lambda (sub-bind bindings)
    (andmap (lambda (bind)
              (not (symbol=? (binding-id sub-bind)
                             (binding-id bind))))
            bindings)))

(define double-bindings?
  (lambda (bs)
    (ormap (lambda (b) (not (false? b))) 
           (let loop ((b (car bs)) (bss (cdr bs)) (res '()))
             (if (empty? bss)
                 res
                 (loop (car bss) 
                       (cdr bss) 
                       (cons (ormap (lambda (bind)
                                      (symbol=? (binding-id b) (binding-id bind)))
                                    bss)
                             res)))))))

(define subst-bindings
  (lambda (env-bind bindings)
    (map (lambda (inner-bind)
           (binding (binding-id inner-bind)
                    (subst env-bind 
                           (binding-expr inner-bind))))
         bindings)))

(define-type WAEE
  (num (n number?))
  (op (proc symbol?))
  (binop (proc op?) (lhs WAEE?) (rhs WAEE?))
  (with (bindings bindings-list?) (body WAEE?))
  (binding (id symbol?) (expr WAEE?))
  (id (name symbol?)))

(define parse-waee
  (lambda (sexp)
    (cond ((number? sexp) (num sexp))
          ((symbol? sexp) (id sexp))
          ((and (list? sexp) 
                (= (length sexp) 3)) (case (car sexp)
                                       ((+) (binop (op 'add)
                                                   (parse-waee (cadr sexp))
                                                   (parse-waee (caddr sexp))))
                                       ((-) (binop (op 'sub)
                                                   (parse-waee (cadr sexp))
                                                   (parse-waee (caddr sexp))))
                                       ((*) (binop (op 'mult)
                                                   (parse-waee (cadr sexp))
                                                   (parse-waee (caddr sexp))))
                                       ((/) (binop (op 'div)
                                                   (parse-waee (cadr sexp))
                                                   (parse-waee (caddr sexp))))
                                       ((with) (with (map (lambda (b)
                                                            (binding (car b) 
                                                                     (parse-waee (cadr b))))
                                                          (cadr sexp))
                                                     (parse-waee (caddr sexp))))
                                       (else (error 'parse-waee "malformed arithmetic expression"))))
          (else (error 'parse-waee "malformed arithmetic expression")))))

(define subst
  (lambda (env-bind expr)
    (type-case WAEE expr
      (num (n) (num n))
      (binop (p l r) (binop p (subst env-bind l) (subst env-bind r)))
      (with (bindings bound-body)
            (if (bindable-body? env-bind bindings)
                (with (subst-bindings env-bind bindings)
                      (subst env-bind bound-body))
                (with (subst-bindings env-bind bindings)
                      bound-body)))
      (id (v) (if (symbol=? v (binding-id env-bind))
                  (binding-expr env-bind)
                  (id v)))
      (else (error 'subst "malformed binding expression")))))

(define interp-waee
  (lambda (waee)
    (type-case WAEE waee
      (num (n) n)
      (op (p) (cadr (assoc p op-table)))
      (binop (p l r) ((interp-waee p) (interp-waee l) (interp-waee r)))
      (with (bs e) (if (double-bindings? bs)
                       (error 'interp-waee "identifier(s) bound twice")
                       (interp-waee (foldr subst e (map interp-waee bs)))))
      (binding (i e) (binding i (num (interp-waee e))))
      (id (_) (error 'interp-waee "free identifier remains in expression")))))

(define eval-waee
  (lambda (sexp)
    (interp-waee (parse-waee sexp))))