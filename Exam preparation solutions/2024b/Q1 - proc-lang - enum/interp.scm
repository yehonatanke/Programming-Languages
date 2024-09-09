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

        ;; Solution 

               (enum-exp (ids)
                         (if (null? ids)
                            (eopl:error "enum list must not be empty")
                           (enum-val (enumerate ids))))

   ;     (enum-exp (ids)
    ;              (if (null? ids)
     ;                 (eopl:error 'enunm-exp "Cannot initialize empty enum")
      ;                (let ((idlist (convert-ids ids)))
       ;                 (enum-val (enumerate idlist)))))

        (enum-element-exp (enm id)
                          ; if enm is not enum-val, or id is not a enm type - error
                          (let* ((enm-val (value-of enm env)) 
                                 (id-val (value-of id env)))
                            ; check if id is one of enm values
                            (if (equal? (car enm-val) id)
                                (identifier-val id-val)  ; return the id representation, i.g identifier-val 'green
                                (eopl:error "~s is not ~s value" id enm))))

        (match-exp (enmid exp1 enmids exps)
                   (let* ((enm-val (expval->enum (value-of enmid env)))
                          ;(exp1-val (value-of exp1 env))
                          (id-val (expval->identifier-val (value-of exp1 env))))
                     ; check valid enmids exps
                     (cond
                       ((not (member? enm-val id-val))
                        (eopl:error "enmids is not member of ~s" enmid)) ; not a member     
                       ((or (not (equal-lists? enm-val enmids)) (not (equal? (length enm-val) (length enmids)))) ; lists are different
                        (eopl:error "enmids is not valid enum for ~s" enm-val))                      
                       (else (apply-enum enm-val id-val env exps)))))

        )))

  ;; Solution Utility

  (define member?
    (lambda (a lst)
      (cond ((null? lst) #f)
            ((equal? (car lst) a) #t)
            (else (member a (cdr lst))))))

  (define (equal-lists? lst1 lst2 lst2-length)
    (cond
      ((null? lst2) #t) ; list are empty
      ((member? lst1 (car lst2)) (equal-lists? (cdr lst2)))
      (else #f)))

  (define (apply-enum lst1 exp lst2 env)
    (cond
      ((null? lst1) (eopl:error "enum is not valid"))
      ((equal? (car lst1) exp) (value-of (car lst2) env))
      (else (apply-enum (cdr lst1) exp (cdr lst2) env))))

    (define convert-ids
    (lambda (ids)
      (if (null? ids)
          '()
          (cons (identifier-val (car ids)) (convert-ids (cdr ids))))))
    
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of body (extend-env var val saved-env))))))

  )
