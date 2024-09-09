(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var)
                 (deref (apply-env env var)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t) 
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

#|
need to:
1. calculate value of exp1
2. calculate exp1 type
3. go through `type-list` and: (also save index as i)
 3.1 look for match between exp1-type and `type-list`
  3.1.1 if true: calculate the value of `bool-lst` at index i
   3.1.2 if true: return the value of the expression of `exp-lst` at index i


      (switch-exp (exp1 type-lst id-lst bool-lst exp-lst de-fexp)
                    (define switch-helper
                      (lambda (exp1 type-lst id-lst bool-lst exp-lst de-fexp index)
                        (if (zero? (index))
                            (value-of de-fexp env) ; base case - the default expression
                            
                            (let* ((exp1-val (value-of exp1 env)) 
                                   (exp1-type (the-exp-type exp1-val))
                                   (id1-type (the-id1-type id-lst)))
                              (cond
                                ((zero? (- id1-type exp1-type)) (check-bools-list bool-lst env))
|#
        (switch-exp (exp1 type-lst id-lst bool-lst exp-lst de-fexp)
                    (if (zero?(length type-lst))
                        (eopl:error "empty type in switch is not allowed")
                        ; (switch-helper exp1 type-lst id-lst bool-lst exp-lst de-fexp env 0)
                        (switch-helper (value-of exp1 env) type-lst id-lst bool-lst exp-lst de-fexp env 0)
                        ; (apply-switch env (value-of exp1 env) type-lst id-lst bool-lst exp-lst de-fexp)
                        ))
                
        )))
  ;; other solution
  (define (apply-switch base-env curr-val types ids bools exps de-fexp)
    (if (null? types)
        (value-of de-fexp base-env)
        (if (matching-types curr-val (car types))
            (let ((new-env (extend-env (car ids) (newref curr-val) base-env)))
              (if (expval->bool (value-of (car bools) new-env))
                  (value-of (car exps) new-env)
                  (apply-switch base-env curr-val (cdr types) (cdr ids) (cdr bools) (cdr exps) de-fexp)))
            (apply-switch base-env curr-val (cdr types) (cdr ids) (cdr bools) (cdr exps) de-fexp))))

  (define (matching-types exp1 t)
    (cases expval exp1
      (num-val (num) (cases type t (number-type () #t) (else #f)))
      (bool-val (bool) (cases type t (boolean-type () #t) (else #f)))
      (proc-val (proc) (cases type t (function-type () #t) (else #f)))
      (else #f)))

  ;; debugging
  (define switch-helper
    (lambda (exp1 type-lst id-lst bool-lst exp-lst de-fexp env idx)
      (begin
        (display "Entering switch-helper (idx: ") (display idx) (display ")\n")
        (display "type-lst: ") (display type-lst) (newline)
        (if (null? type-lst)
            (begin
              (display "Reached base case, evaluating default expression\n")
               (value-of de-fexp env))
            (let* ((exp1-type (the-exp-type exp1 env))
                   (id1-type (the-id1-type (car type-lst))))
              (begin
                (display "exp1-type: ") (display exp1-type)
                (display ", id1-type: ") (display id1-type) (newline)
                (if (zero? (- id1-type exp1-type))
                    (begin
                      (display "Types match. Checking boolean expression.\n")
                      (if (expval->bool  (value-of (car bool-lst) env))
                          (begin
                            (display "Boolean expression is true. Setting reference.\n")
                            (value-of (car exp-lst) (extend-env (car id-lst) (newref exp1) env)))
                          (begin
                            (display "Boolean expression is false. Moving to next case.\n")
                            (switch-helper exp1 (cdr type-lst) (cdr id-lst) (cdr bool-lst) (cdr exp-lst) de-fexp env (+ idx 1)))))
                    (begin
                      (display "Types don't match. Moving to next case.\n")
                      (switch-helper exp1 (cdr type-lst) (cdr id-lst) (cdr bool-lst) (cdr exp-lst) de-fexp env (+ idx 1))))))))))



  #|
  (define switch-helper1
    (lambda (exp1 type-lst id-lst bool-lst exp-lst de-fexp env idx)
      (if ;(zero? (length type-lst))
       (null? type-lst)
          (deref (value-of de-fexp env)) ; base case - the default expression
          ; else - calculate the next type
          (let*  
              ((exp1-type (the-exp-type exp1 env)) ; expression1 type
               (id1-type (the-id1-type (car type-lst)))) ; identifier1 type
            (if (zero? (- id1-type exp1-type))
                ; if the types are match:
                ; 1. check if the correspond expression is true
                (if (expval->bool (deref (value-of ((car bool-lst) env))))
                    ; 2. calculate exp-lst(i) with the id-list(i) and the first exp1
                    (setref!
                     (apply-env env (car id-lst))
                     (value-of exp1 env))
                    (switch-helper exp1 (cdr type-lst) (cdr id-lst) (cdr bool-lst) (cdr exp-lst) env de-fexp (+ 1 idx)))
                (switch-helper exp1 (cdr type-lst) (cdr id-lst) (cdr bool-lst) (cdr exp-lst) env de-fexp (+ 1 idx)))))))

|#

  
#|
let x=15...
switch proc (b) -(b,4)
    {
    boolean i when (if i then zero? (x) else zero?(0) ) => 90,
    number a when (zero?(x)) => 100,
    function f when (zero?((-(x,15))) => (f 300),
    number i when (zero? ((-(i,11))) => 200,
    default => 400
    }
|#

  
  ;; --- Q8 Utility ---
  (define the-exp-type
    (lambda (exp1 env) 
      (cases expval  (value-of exp1 env)
        (num-val (num) 1)
        (bool-val (bool) 2)
        (proc-val (proc) 3)
        (else 5)
        )))

  (define the-id1-type
    (lambda (id-lst)
      (cases type id-lst
        (number-type () 1)
        (boolean-type () 2)
        (function-type () 3)
        (else 4)
        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (let ((r (newref arg)))
            (let ((new-env (extend-env var r saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    var)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))  

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
