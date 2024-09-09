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

        ; Solution
        #|
        ; Solution for proc-exp from the book
        (proc-exp (var statvars statvals body)
                  (letrec (
                           ; Compute the values of static variables
                           (vals (map (lambda (exp) (value-of exp env)) statvals))
    
                           ; Recursive function to extend the environment with static variables
                           (expand-env 
                            (lambda (vars vals env1)
                              (if (null? vars)
                                  env1  ; If no more variables, return the current environment
                                  ; Otherwise, continue extending the environment with the next variable
                                  (expand-env 
                                   (cdr vars)  ; Rest of the variables
                                   (cdr vals)  ; Rest of the values
                                   (extend-env 
                                    (car vars)  ; Current variable
                                    (newref (car vals))  ; Create a new reference for the current value
                                    env1)))))  ; Extended environment so far
    
                           ; Create a new environment with static variables
                           (env-with-static (expand-env statvars vals env)))
    
                    ; Create a procedure value with the parameter, function body, and extended environment
                    (proc-val (procedure var body env-with-static))))

  |#      
        (proc-exp (var statvars statvals body)
                  (if (null? statvars)
                      (proc-val (procedure var body env))
                      (let ((new-env (update-static statvars statvals env)))
                        (proc-val (procedure var body new-env)))))

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
 #|
        (assign-exp (var exp1)
                    (begin
                      ;; Debugging: Print the variable name and its current value in the environment
                      (display "Assigning to variable: ") (display var) (newline)
                      (display "Current value of variable in env: ")
                      (display (apply-env env var)) (newline)
    
                      ;; Look up the reference of `var` in the environment `env`,
                      ;; and update its value with the result of evaluating `exp1`.
                      (let ((new-value (value-of exp1 env)))
                        ;; Debugging: Print the new value to be assigned
                        (display "New value of exp1: ") (display new-value) (newline)
      
                        ;; Perform the assignment
                        (setref! (apply-env env var) new-value))
    
                      ;; Debugging: Confirm assignment
                      (display "Assignment complete. Returning value 27.") (newline)
    
                      ;; Return a numerical value of 27 after assignment.
                      (num-val 27)))
|#

       
        (assign-exp (var exp1)
                    (begin
                      (setref!
                       (apply-env env var)
                       (value-of exp1 env))
                      (num-val 27)))

        )))

  ; Solution Utility

  (define update-static
    (lambda (var-lst val-lst env)
      (if (null? var-lst)
          env
          (let ((ext-var (car var-lst))
                (ext-val (value-of (car val-lst) env)))
            (update-static (cdr var-lst) (cdr val-lst) (extend-env ext-var (newref ext-val) env))))))


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
  


  
