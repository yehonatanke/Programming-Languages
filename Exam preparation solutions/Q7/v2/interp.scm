(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the procedural
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

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
        (let-exp (tmps exp1 body)
          (cases temps tmps
            (multi-temps (optional-ids) (handle-multi-let optional-ids exp1 body env))  ; handle the new "let" expression
            (one-temp (var)  ; just a regular let expression with one var
          (let ((val1 (value-of exp1 env)))  ; the usual handling for regular "let"
            (value-of body
              (extend-env var val1 env))))))
        
        
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))
        
        (tuple-exp(exps)
                  (tuple-val (map (lambda (exp) (value-of exp env)) exps)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        )))
  
  ;; helper function to handle the new let expression
  ;; list of optional ids X expression(should be tuple) X expression X environment -> expval
  
  (define (handle-multi-let optional-ids exp1 body env)
    (let* ((tuple-expval (value-of exp1 env))
           ; extract the expvals list from the tuple expval
          (exps-list (cases expval tuple-expval  
                       (tuple-val (tuple) tuple)
                       (else (eopl:error "error: expected a tuple for let expression"))))
          (new-env (extend-optionals optional-ids exps-list env))) ; create new environment with matching identifiers + values
      ; check if parameters are valid (could have done this check even before extending the environment, doesnt really matter)
      (if (not (= (length exps-list) (length optional-ids)))
          (eopl:error "length of tuples and temps not the same")
      (if (not (has-id optional-ids))
          (eopl:error "must have at least one id in temps")
      (value-of body new-env)))))
  
  


  ; helper function to extend the environment with the id's ( if not "_") and matching exp
  ; List of optional-identifiers X list of expvals X environment -> environment
  
  (define (extend-optionals opts exps env)
    (if (null? opts)
        env
        (cases optional-identifier (car opts)
          (ignore () (extend-optionals (cdr opts) (cdr exps) env))
          (id (identifier) (extend-optionals (cdr opts) (cdr exps) (extend-env identifier (car exps) env))))))
  
  ; helper function , returns #t if there is at least one identifier in the optional-identifier list
  ; 
  (define (has-id opts)
    (if (null? opts)
        #f
    (cases optional-identifier (car opts)
      (ignore () (has-id (cdr opts)))
      (id (identifier) #t))))


  ;; procedure : Var * Exp * Env -> Proc
  ;; Page: 79
  (define procedure
    (lambda (var body env)
      (lambda (val)
        (value-of body (extend-env var val env)))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc val)
      (proc val)))

  )
