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
        (var-exp (var) (deref (apply-env env var)))

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
                       (apply-env env var) ;; Find the variable in the environment
                       (value-of exp1 env)) ;; Evaluate the new value and store it
                      (num-val 27)))
#|
        ;; Purpose: Temporarily set a variable to a new value, execute a body of code,
        ;; then restore the variable to its original value.
        (setdynamic-exp (var exp1 body)
                        (let*
                            ((varref (apply-env env var))    ; Get the reference (location) of the variable
                             (oldval (deref varref))         ; Store the original value of the variable
                             (tempval (value-of exp1 env)))  ; Evaluate the new temporary value
                          (begin
                            ;; Step 1: Set the variable to the new temporary value
                            (setref! varref tempval)
      
                            ;; Step 2: Evaluate the body with the temporary value in effect
                            (let ((result (value-of body env)))
                              (begin
                                ;; Step 3: Restore the variable to its original value
                                (setref! varref oldval)
                                ;; Step 4: Return the result of the body evaluation
                                result)))))
|#
        
        ;; Purpose: Temporarily set a variable to a new value, execute a body of code,
        ;; then restore the variable to its original value.
        (setdynamic-exp (var exp1 body)
                        (let*
                            ((varref (apply-env env var))    ; Lookup the reference of `var` in the environment
                             (oldval (deref varref))         ; Store the original value of `var` (so we can restore it later)
                             (tempval (value-of exp1 env)))  ; Evaluate the new temporary value (exp1) for `var` in the environment
                          (begin
                            ; Step 1: Temporarily set `var` to the new value `tempval`
                            (setref! varref tempval)
                            ; Step 2: Evaluate the body of the expression while `var` is bound to the new value
                            (let ((result (value-of body env)))
                              (begin
                                ; Step 3: Restore the original value of `var` after the body has been evaluated
                                (setref! varref oldval)
                                ; Step 4: Return the result of evaluating the body
                                result)))))


                                       

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
  


  
