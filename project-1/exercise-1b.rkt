#lang plai
(require racket/trace)

(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (name symbol?) (named-expr WAE?) (body WAE?))
  (id (name symbol?)))

(define parse-wae
  (lambda (sexp)
    (cond ((number? sexp) (num sexp))
          ((symbol? sexp) (id sexp))
          ((list? sexp) (case (car sexp)
                          ((+) (add (parse-wae (cadr sexp))
                                    (parse-wae (caddr sexp))))
                          ((-) (sub (parse-wae (cadr sexp))
                                    (parse-wae (caddr sexp))))
                          ((with) (with (caadr sexp)
                                        (parse-wae (cadadr sexp))
                                        (parse-wae (caddr sexp))))
                          (else (error 'parse-wae "malformed arithmetic expression"))))                         
          (else (error 'parse-wae "malformed arithmetic expression")))))

(define subst
  (lambda (expr sub-id val)
    (type-case WAE expr
      (num (n) (num n))
      (add (l r) (add (subst l sub-id val) (subst r sub-id val)))
      (sub (l r) (sub (subst l sub-id val) (subst r sub-id val)))
      (with (bound-id named-expr bound-body)
            (if (symbol=? bound-id sub-id)
                (with bound-id
                      (subst named-expr sub-id val)
                      bound-body)
                (with bound-id 
                      (subst named-expr sub-id val) 
                      (subst bound-body sub-id val))))
      (id (v) (if (symbol=? v sub-id)
                  val
                  (id v))))))

(define interp-wae
  (lambda (wae)
    (type-case WAE wae
      (num (n) n)
      (add (l r) (+ (interp-wae l) (interp-wae r)))
      (sub (l r) (- (interp-wae l) (interp-wae r)))
      (with (i v e) (interp-wae (subst e i (num (interp-wae v)))))
      (id (v) (error 'calc "free identifier remains in expression")))))

(define eval-wae
  (lambda (sexp)
    (interp-wae (parse-wae sexp))))
