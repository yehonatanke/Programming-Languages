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

        ;; const
        (const-exp (num) (num-val num))

        ;; var
        (var-exp (var) (apply-env env var))

        ;; diff
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))

        ;; zero
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
              
        ;; if
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))

        ;; let
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))

        ;; proc
        (proc-exp (var types bodies)
                  ;; check for valid input
                  (if (and (valid-types? types) (not (null? bodies)))
                      (proc-val (procedure var types bodies env))
                      (begin
                        (if (null? bodies) 
                            (eopl:error 'proc-exp "Procedure definition error - no body")
                            (eopl:error 'proc-exp "Invalid types provided for procedure"))
                        )))


        ;; call
        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        )))

  ;; --- Utility Functions --- ;;

  ;; valid-types? : Listof String -> Bool
  ;; Checks if the list of types is valid, ensuring no duplicates or invalid types.
  (define valid-types?
    (lambda (types)
      (cond
        ;; Check if types is empty
        ((null? types)
         (begin
           (eopl:error 'valid-types? "Procedure definition error - no body")
           #f))
        ;; Check for invalid or duplicate types
        (else
         (let ((valid? #t)) ;; Initialize valid? as true
           (let ((seen '())) ;; Initialize seen as an empty list
             (for-each
              (lambda (type)
                (if (or (not (member type '("Int" "Bool" "Proc"))) ;; Check if type is invalid
                        (member type seen)) ;; Check if type is a duplicate
                    (begin
                      (set! valid? #f) ;; Set valid? to false
                      (if (not (member type '("Int" "Bool" "Proc")))
                          (eopl:error 'valid-types? "Invalid type found. Only Int, Bool, Proc are allowed" ) ;; Error for invalid type
                          (eopl:error 'valid-types? "Procedure definition error - duplicate types" )) ;; Error for duplicate type
                      )
                    (set! seen (cons type seen)))) ;; Add type to seen list if valid
              types))
           valid?))))) ;; Return valid? status
  

  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var types bodies saved-env)
                   (let ((body (find-body-for-type types bodies val)))
                     (value-of body (extend-env var val saved-env))))))) 


  
  ;; find-body-for-type : Listof String * Listof Expression * ExpVal -> Expression
  (define find-body-for-type
    (lambda (types bodies val)
      (let ((type (expval-type val)))
        (let loop ((types types) (bodies bodies))
          (cond
            ((or (null? types) (null? bodies)) (eopl:error 'find-body-for-type "There is no body for such parameter"))
            ((equal? (car types) type) (car bodies))
            (else (loop (cdr types) (cdr bodies))))))))



  ;; expval-type : ExpVal -> String
  (define expval-type
    (lambda (val)
      (cases expval val
        (num-val (num) "Int")
        (bool-val (bool) "Bool")
        (proc-val (proc) "Proc")
        (else (eopl:error 'expval-type "Unknown ExpVal type" val)))))


  )
