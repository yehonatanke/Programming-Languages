(module interp (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

  ;; drafts

  ; util to `check-exp-num`
  (define exp-counter-draft
    (lambda (lst)
      (if (null? lst)
          0
          (if (symbol? (car lst))
              (+ 1 (exp-counter (cdr lst)))
              (exp-counter (cdr lst))))))

  ;; (Q7) - utility functions - old version

  (define one-temp?
    (lambda (x)
      (symbol? x)))

  (define many-temp?
    (lambda (x)
      ((list-of optional-identifier?) x)))

  ; check for valid tuple extraction (return #f for valid input) -- (`unvalid-input` helper)
  (define unvalid-input
    (lambda (l-exp r-exp)
      (letrec ((equality-valid (equal? (counter l-exp) (counter r-exp)))
               (exp-num-valid (check-exp-num l-exp))) 
        (cond
          ((not (equality-valid)) (eopl:error 'let-exp "tuple extraction unequal number of variable"))
          ((not (exp-num-valid)) (eopl:error 'let-exp "tuple extraction with less than one variable"))
          (else #f)))))

  ; check for valid number of expressions (at least 1) -- (`unvalid-input` helper)
  (define check-exp-num
    (lambda (l-exp)
      (let ((count (exp-counter l-exp))) 
        (if (zero? count) #f
            #t))))

  ; util to `check-exp-num` -- (`unvalid-input` helper)
  (define exp-counter
    (lambda (lst)
      (cond
        ((null? lst) 0)
        ((symbol? (car lst)) (+ 1 (exp-counter (cdr lst))))
        (else (exp-counter (cdr lst))))))

  ; general counter -- (`unvalid-input` helper)
  (define counter
    (lambda (lst)
      (if (null? lst)
          0
          (+ 1 (counter (cdr lst))))))

  ; extract variables from tuple and update env 
  (define handle-tuple 
    (lambda (l-exp r-exp env)
      (if (check-exp-num l-exp) ; true if there is at least 1 identifier
          (letrec 
              ((idx (extract-index l-exp 0)) ; variable index
               (var (extract-var l-exp)) ; variable name
               (val (extract-val r-exp idx env))) ; the value correspond to the variable index 
            (begin
              ((extended-env-record var val env)) ; update env 
              ((handle-tuple (cdr l-exp) r-exp)))) ; recursive call 
          #f)) ; end of `if`
    ; else
    )

  (define extract-index
    (lambda (lst counter)
      (cond
        ((null? lst) #f)
        ((symbol? (car lst)) counter) 
        (else (extract-index (cdr lst) (+ 1 counter))))))
  
  (define extract-var
    (lambda (lst)
      (cond
        ((null? lst) #f)
        ((symbol? (car lst)) (car lst)) ; if first var is symbol than its identifier. return it
        (else (extract-var (cdr lst))))))

  (define extract-val
    (lambda (lst idx)
      (cond
        ((null? lst) #f)
        ((zero? idx) (car lst)) 
        (else (extract-val (cdr lst) (- 1 idx))))))
             

      
      
    
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
                      (num-val (- num1 num2)))))

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

        ;; (Q7) - new version
        (let-exp (l-exp r-exp body)
                 ;; Evaluate the right-hand side expression in the current environment
                 (let* ((val1 (value-of r-exp env))
                        ;; Create a new environment for the body based on the type of l-exp
                        (body-env (cases temps l-exp
                                    ;; If l-exp is a single identifier, extend the environment with it
                                    (one-temp (id) (extend-env id val1 env))
                                    ;; If l-exp is multiple identifiers, apply them to the tuple value
                                    (many-temp (opt-ids) (apply-let opt-ids (expval->tuple val1) env)))))
                   ;; Evaluate the body in the new environment
                   (value-of body body-env)))
        
        #|
        ;; old version
        (let-exp (l-exp r-exp body)
                 ; (if (expval->bool (value-of (one-temp var) env))         
                 (letrec
                     ((temp-type (value-of l-exp env)))
                   ; Find out which case it is
                   (cond
                     ; true if there is only one identifier (one-temp)
                     ((one-temp? temp-type)
                      (let ((val1 (value-of r-exp env)))
                        (value-of body (extend-env l-exp val1 env))))
               
                     ; true if there is many identifiers (many-temp)
                     ((many-temp? temp-type)
                      (if (unvalid-input l-exp r-exp)
                          ; unvalid input: return an error message
                          (eopl:error 'let-exp "unvalid input ~s ~s" l-exp r-exp) 
                          ; else - valid input: update new values and activate `in` part of `let`
                          (begin 
                            (handle-tuple l-exp r-exp env)
                            (value-of body env))))
                     (else (eopl:error 'let-exp "let expression error"))

                     ; Need to:
                     ; 1. check for valid input (number of variable equality, at least one identifier which is not '_')
                     ; 2. extract every variable from the left side and update its values according to the corresponding expression from the right side
                     ; 3. return the "in" expression of the 'let'
                     ; 4. print error message if reach the end (no condition was true)

                     )))

        
        ;; (Q7) - old version
        (tuple-exp (exps)
                   ;; Create a tuple value from the given expressions
                   (tuple-val 
                    ;; Create a tuple constant from the list of expressions
                    ;; Note: tuple-const likely evaluates each expression in exps
                    ;; and creates a constant tuple from the resulting values
                    (tuple-const exps)))
        ;---------
        |#
        
        (proc-exp (var body)
                  (proc-val (procedure var body env)))

        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        ;; (Q7) - new version
        (tuple-exp (exps)
                   ;; Evaluate each expression in the tuple within the current environment
                   (let ((vals (map (lambda (x) (value-of x env)) exps)))
                     ;; 'vals' now contains the evaluated results of all expressions

                     ;; Check if the resulting tuple would be empty
                     (if (= (length vals) 0)
                         ;; If it's empty, throw an error because empty tuples are not allowed
                         (eopl:error "tuple can't be empty")
                         ;; If it's not empty, create and return a tuple value
                         ;; 'tuple-val' is likely a constructor for tuple values in this interpreter
                         (tuple-val vals))))

        )))

  ;; (Q7) - new version util function
  (define (apply-let vars vals env)
    ;; Helper function to recursively process variables and values
    (define (apply-let-impl vars vals env count)
      ;; Check if the number of variables matches the number of values
      (if (= (length vars) (length vals))
          (if (null? vars)
              ;; If we've processed all vars/vals, check if any bindings were made
              (if (zero? count)
                  (eopl:error "Empty tmps in let is disallowed")
                  env)
              ;; Process the current variable
              (cases optional-identifier (car vars)
                ;; If it's an ignore placeholder, skip this binding
                (ignore () 
                        (apply-let-impl (cdr vars) (cdr vals) env count))
                ;; If it's an identifier, extend the environment with the new binding
                (id (var) 
                    (apply-let-impl (cdr vars) (cdr vals) 
                                    (extend-env var (car vals) env) 
                                    (+ count 1)))))
          ;; Error if vars and vals have different lengths
          (eopl:error "mismatch between amount of identifiers and values")))
  
    ;; Start the recursive process with an initial count of 0
    (apply-let-impl vars vals env 0))


  

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of body (extend-env var val saved-env))))))

  )
