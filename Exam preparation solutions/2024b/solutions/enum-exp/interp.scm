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

        (enum-exp (ids)
                  (if (null? ids)
                      (eopl:error 'enunm-exp "Cannot initialize empty enum")
                      (let ((idlist (convert-ids ids)))
                        (enum-val (enumerable idlist)))))

        (enum-elmt-exp (enm id)
                       (let ((enm-val (value-of enm env)))
                         (cases expval enm-val
                           (enum-val (enm1)
                                     (cases enum enm1
                                       (enumerable (ids) (in-enum ids id))))
                           (else
                            (eopl:error 'enum-elmt-exp "~s is not an enum" enm)))))

        (match-exp (enmid exp1 enmids exps)
                   (let* ((cid (expval->id(value-of exp1 env)))
                          (enm (expval->enum (apply-env env enmid)))
                          (matching (cases enum enm
                                      (enumerable (ids)
                                                  (all-match? ids enmids)))))
                     (if matching
                         (apply-match cid enmids exps env)
                         (eopl:error 'match-exp "Given ids doesn't match enum ids"))))
             
         
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of body (extend-env var val saved-env))))))

  (define convert-ids
    (lambda (ids)
      (if (null? ids)
          '()
          (cons (identifier-val (car ids)) (convert-ids (cdr ids))))))

  (define deconvert-ids
    (lambda (ids)
      (if (null? ids)
          '()
          (cons (expval->id (car ids)) (deconvert-ids (cdr ids))))))

  (define in-enum
    (lambda (ids id)
      (if (null? ids)
          (eopl:error 'in-enum "~s is not a part of the enum" id)
          (let ((idval (expval->id (car ids))))
            (if (equal? id idval)
                (car ids)
                (in-enum (cdr ids) id))))))

  (define all-match?
    (lambda (en-ids ext-ids)
      (let ((enm-ids (deconvert-ids en-ids)))
        (if (fully-in? ext-ids enm-ids)
            (if (fully-in? enm-ids ext-ids)
                #t
                (eopl:error 'all-match? "Not all enum ids are in match case"))
            (eopl:error 'all-match? "There is an illegal external id")))))

  (define fully-in?
    (lambda (ids1 ids2)
      (if (null? ids1)
          #t
          (if (member (car ids1) ids2)
              (fully-in? (cdr ids1) ids2)
              #f))))

  (define apply-match
    (lambda (id ids exps env)
      (if (null? ids)
          (eopl:error 'apply-match "Cannot do match on empty id set")
          (if (equal? id (car ids))
              (value-of (car exps) env)
              (apply-match id (cdr ids) (cdr exps) env)))))

  )
