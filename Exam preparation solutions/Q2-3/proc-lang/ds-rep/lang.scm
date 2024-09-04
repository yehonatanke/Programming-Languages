(module lang (lib "eopl.ss" "eopl")                

  ;; grammar for the PROC language
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
       ("-" "(" expression "," expression ")")
       diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)
      
      ))

  ;; for Q3
  
  ;; > (scan&parse "((proc (x) proc (x) (x 12) 7) proc (x) -(x,8))")
  ;; (a-program (call-exp (call-exp (proc-exp 'x (proc-exp 'x (call-exp (var-exp 'x) (const-exp 12)))) (const-exp 7)) (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 8)))))


  (define (apply-scan-and-parse input)
    (display "Input expression: ")
    (display input)
    (newline)
    (display "Parsed result: ")
    (display (scan&parse input))
    (newline))
  ;(apply-scan-and-parse "((proc (x) proc (x) (x 12) 7) proc (x) -(x,8))")

  (define (apply-scan-and-parse-Q3)
    (display "Parsed result:")
    (newline)
    (display (scan&parse "((proc (x) proc (x) (x 12) 7) proc (x) -(x,8))"))
    (newline)
    (newline)
    (display "Parsed result:")
    (newline)
    (scan&parse "((proc (x) proc (x) (x 12) 7) proc (x) -(x,8))")
    )

  ; "(proc (x) -(x, 12) 12 )"

  ; (apply-scan-and-parse-v2 "(proc (x) -(x, 12) 12 )")
    (define (apply-scan-and-parse-v2 input)
    (display "Parsed result:")
    (newline)
    (display (scan&parse input))
    (newline)
    (newline)
    (display "Parsed result:")
    (newline)
    (scan&parse input)
    )

  ; (scan&parse "((proc (x) proc (y) -(x,y)  5) 6)")

  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
