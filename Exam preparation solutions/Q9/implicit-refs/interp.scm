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

        ; procedures
        (proc-exp (var body)
                  (proc-val (procedure var body env)))

        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))

        ; recursive let
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

        ;;; Q9

        ; the generator 
        (gen-exp (var exps retexp)
                 (let ((p (procedure var retexp env))
                       (vals (map (lambda (x) (value-of x env)) exps)))                       
                   (gen-val (generate p vals))))

        ; return the generator
        (return-exp (id)
                    (let* ((ref1 (apply-env env id))
                           (g1 (deref ref1)))
                      (cases expval g1
                        (gen-val (gen)
                                 (cases generator gen
                                   (generate (pr1 vals)
                                             (if (null? vals)
                                                 (eopl:error 'return-exp "Generator is empty")
                                                 (let ((retval (apply-procedure pr1 (car vals))))
                                                   (begin
                                                     (setref! ref1 (gen-val (generate pr1 (cdr vals))))
                                                     retval))))))
                        (else (eopl:error 'return-exp "~s not a generator" id)))))
                       

        ; empty generator?
        (empty-exp (id)
                   (let* ((ref1 (apply-env env id))
                          (g1 (deref ref1)))
                     (cases expval g1
                       (gen-val (gen)
                                (cases generator gen
                                  (generate (pr1 vals)
                                            (bool-val (null? vals)))))
                       (else (eopl:error 'return-exp "~s not a generator" id)))))                

        ))) 

;; --- Util Functions for Q9 ---

; extract the first element from the list (update the list to be without this element) - call it `element1`
; extract the function of the generator from the generator - call it `gen1`
; if the element doesn't exist, print error message
; if the generator doesn't exist, print error message
; apply '(apply-procedure gen1 element1)'

#|
;; next-generator : Ref -> ExpVal
;; This function advances a generator to its next value
(define apply-generator
  (lambda (gen-ref)
    (let ((generator-val (deref gen-ref)))
      (cases gen generator-val
        (generator (exps retexp)
                   (if (is-empty-generator gen-ref)
                       (eopl:error "Generator is empty")
                       (let ((element1 (car exps))
                             (remaining-exps (cdr exps)))
                         ;; Update the generator in the store with remaining expressions
                         (setref! gen-ref (gen-val (generator remaining-exps retexp)))
                         (apply-procedure retexp element1))))))))  

(define is-empty-generator
  (lambda (gen-ref)
    (let ((generator-val (deref gen-ref)))
      (cases gen generator-val
        (generator (exps retex)
                   (if (null? exps)
                       (begin (eopl:error "Generator is empty")  ;; Error: no elements
                              #t)
                       #f))))))
  |#
#|
  (define apply-generator-draft
    (lambda (gen-ref)
      (let ((generator-val (deref gen-ref)))
        (cases gen generator-val
          (generator (var exps retex)
                     (if (null? exps)
                         (begin (eopl:error "Generator is empty")  ;; Error: no elements
                                #f)
                         (let ((element1 (car exps))))
                         (remaining-exps (cdr exps)))
                     ;; Update the generator in the store with remaining expressions
                     (setref! gen-ref (gen-val (generator var remaining-exps retex)))
                     (if (null? retex)
                         (begin (eopl:error 'next-generator "Gen-extract: ~s Not a generator")  ;; Error: no elements
                                #f)
                         (apply-procedure gen1 element1)))                    
          (else (eopl:error 'next-generator "Not a generator: ~s" generator-val))))))
  |#
; --- End of Q9 ---


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
  


  
