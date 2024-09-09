(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
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
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))

        ; Solution

        (proc-exp (ids de-fs body)
                  (let ((defs1 (map (lambda (x) (value-of x env)) de-fs)))
                  (proc-val (procedure ids defs1 body env))))

        (call-exp (rator parms exps)
                  (let ((proc1 (expval->proc (value-of rator env))))
                    (cases proc proc1
                      (procedure (p-ids p-defs p-body p-env)
                                 (if (and
                                      (not (null? parms)) ; parameter list is not empty
                                      (not (all-member? parms p-ids))) ; parameter list have a variable not in p-ids
                                     (eopl:error "wrong arguments to procedure")
                                     ; else - apply the procedure - the input is valid 
                                     (apply-procedure-sol proc1 parms exps p-ids p-defs p-body p-env))))))


        )))

  ; Solution

  ; util - member and all-member
  (define member?
    (lambda (a lst)
      (cond ((null? lst) #f)
            ((equal? (car lst) a) #t)
            (else (member? a (cdr lst))))))

  ; check if lst1 is included in lst2
  (define all-member?
    (lambda (lst1 lst2)
      (cond
        ((null? lst1) #t)
        ((member? (car lst1) lst2)
         (all-member? (cdr lst1) lst2))
        (else #f))))

  (define apply-procedure-sol
    (lambda (proc1 parms exps p-ids p-defs p-body p-env)
      (cond
        ((null? p-ids) (value-of p-body p-env)) ; base case - value of body with the new environment
        
        ((null? parms) ; there is no parameters that need to be adjust from the imlementation part
         (let* ((new-env (extend-env (car p-ids) (car p-defs) p-env)) ; insert the variable and value to the new environment
                (new-pids (cdr p-ids)) ; update the new identifier list
                (new-pdefs (cdr p-defs))) ; update the new value lust
           (apply-procedure-sol proc1 parms exps new-pids new-pdefs p-body new-env))) ; recursive call
 
        ;; Reach here if the param list is not empty
        (else (let* ((new-env (extend-env (car parms) (car exps) p-env)) ; insert the variable and value to the new environment
                     (new-pids (remove-element p-ids (car parms))) ; update the new identifier list
                     (new-pdefs (remove-element p-defs (car exps)))) ; update the new value lust
                (apply-procedure-sol proc1 (car parms) (car exps) new-pids new-pdefs p-body new-env))))))


  (define (remove-element lst elem)
    (cond
      ((null? lst) '()) ; if list is empty, return empty list
      ((equal? (car lst) elem) (cdr lst)) ; if first element equals `elem`, return rest of the list
      (else (cons (car lst) (remove-element (cdr lst) elem))))) ; otherwise, keep first element and recurse



  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env a)
                   (value-of body (extend-env var val saved-env))))))

  )
