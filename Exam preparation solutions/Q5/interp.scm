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
        
        (proc-exp (var body)
                  (proc-val (procedure var body env)))

        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        ;; Q5 or page 137 in the Madrich
        (fold-exp (proc1 acc vals)
                  (if (null? vals)
                      ;; Base case: If the list is empty, return the accumulated value
                      (value-of acc env)
                      (let ((sum1 (expval->num (value-of acc env)))  ; Extract the current accumulated value
                            (p1 (expval->proc (value-of proc1 env)))) ; Extract the procedure to be applied
                        (let ((arg (value-of (car vals) env))) ; Extract the first element of the list
                          (let ((v1 (expval->num (apply-procedure p1 arg)))) ; Apply the procedure to the argument
                            (let ((total-sum (const-exp (- sum1 (- 0 v1))))) ; Add the result to the total sum
                              ;; Debug print statements to show intermediate values
                              (debug-print "Debug: acc" acc)
                              (debug-print "Debug: sum1" sum1)
                              (debug-print "Debug: car vals" (car vals))
                              (debug-print "Debug: v1" v1)
                              (debug-print "Debug: total-sum" total-sum)
                              (debug-print "Debug: The remaining list (cdr vals)" (cdr vals))
                              ;; Recursive call with updated accumulator and rest of the list
                              (value-of (fold-exp proc1 total-sum (cdr vals)) env)))))))

       
        #|
        ;; second solution
        (fold-exp (exp1 exp2 exps)
                  (letrec
                      ((proc (expval->proc (value-of exp1 env)))  ; Extract the procedure from exp1
                       (acc (expval->num (value-of exp2 env)))    ; Extract the initial accumulator value from exp2
                       (lst (map (lambda (exp) 
                                   (expval->num 
                                    (apply-procedure proc (value-of exp env))))
                                 exps))  ; Apply proc to each expression in exps and convert results to numbers
                       (sum-all (lambda (l)
                                  (if (null? l) 
                                      0 
                                      (+ (car l) (sum-all (cdr l)))))))  ; Helper function to sum a list of numbers
                    (num-val (sum-all lst))))  ; Sum all the results and return as a number value
        |#


        )))

  (define (debug-print message value)
    (eopl:printf "~a: ~a~n" message value))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of body (extend-env var val saved-env))))))

  )
