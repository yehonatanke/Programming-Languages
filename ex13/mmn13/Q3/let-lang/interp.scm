(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)


  ;; ------------------------------------ ;;
  ;; Utility Functions:

  ;; Extracts the exception message from the try block, if any.
  (define (extract-error-message exp env)
    (let ((result (value-of exp env)))
      (if (expval-is-except? result)
          (exception-message (expval->exception result))
          ""))) ;; Returns an empty string if there is no exception.
  

  ;; Sets the try-result based on the matching catch block.
  (define (evaluate-catch-blocks excp-msg catch-msgs excptexps env)
    (let ((index (index-of string=? excp-msg catch-msgs)))
      (if index
          (value-of (list-ref excptexps index) env)
          ""))) ;; Returns an empty string if there is no match.

  
  ;; Extracts exception messages from the catch-err-lst.
  (define (extract-catch-messages catch-err-lst)
    (map (lambda (ex)
           (cases except ex
             (Exception (msg) msg)))
         catch-err-lst)) ;; Converts each exception in the list to its message.


  ;;  Evaluate the try block and handle exception  
  (define (evaluate-try-exp exp excpts excptexps fineexps env)
    (let ((result (value-of exp env)))
      (if (expval-is-except? result)
          (let ((excp-msg (exception-message (expval->exception result)))
                (catch-msgs (extract-catch-messages excpts)))
            (let ((try-result (evaluate-catch-blocks excp-msg catch-msgs excptexps env)))
              (if (string? try-result)
                  (excp-val (Exception excp-msg)) ;; If no match, return the exception.
                  try-result)))
          result))) ;; If no exception, return the result directly.


  ;; End of Utility Functions
  ;; ------------------------------------ ;;

  
  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
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
                      (if (not (number? num1))
                          num1
                          (if(not(number? num2))
                             num2
                             (num-val (- num1 num2))
                             )))))
        ;; zero
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (number? num1)
                           (bool-val (zero? num1))
                           num1))))

        ;; if
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (let ((bool1 (expval->bool val1)))
                    (if (boolean? bool1)
                        (if bool1
                            (value-of exp2 env)
                            (value-of exp3 env))
                        bool1))))

        ;; let
        (let-exp (var exp1 body)
                 (if (and (expression? exp1) (expression? body))
                     (let ((val1 (value-of exp1 env)))
                       (if (expval-is-except? val1)
                           val1
                           (value-of body (extend-env var val1 env))))
                     (excp-val (Exception "general"))))

        ;; throw
        (throw-exp (excpt)
                   (excp-val excpt))
        
        ;; try
        (try-exp (exp1 excpts excptexps fineexps)
                 (if (or
                      (> (length fineexps) 1)
                      (and (null? excptexps) (null? fineexps))
                      (and (null? excptexps) (not (null? fineexps))))
                     (excp-val (Exception "general")) 
                     (let ((result (evaluate-try-exp exp1 excpts excptexps fineexps env)))
                       (if (not (null? fineexps))
                           (begin
                             (value-of (car fineexps) env) ;; Execute the finally block.
                             result)
                           result))))

        )))
  )






  
