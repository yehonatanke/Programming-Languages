(module lang (lib "eopl.ss" "eopl")                
  ;; grammar for the PROC language
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  ;; Lexical specification
  ;; Defines how to tokenize the input
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  ;; Grammar specification
  ;; Defines the structure of the language
  (define the-grammar
    '(
      ;; Program consists of a single expression
      (program
       (expression)
       a-program)

      ;; Expressions

      (expression
       (number)
       const-exp)

      (expression
       ("-" "(" expression "," expression ")")
       diff-exp)

      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)
      
      (expression
       (identifier)
       var-exp)

      ;; Let expression (Q7)
      (expression
       ("let" temps "=" expression "in" expression)
       let-exp)   

      ;; Procedure definition and call
      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      ;; Tuple expression (Q7)
      (expression
       ("<" (separated-list expression ",") ">")
       tuple-exp)
      
      ;; Temporary variable definitions
      (temps
       (identifier)
       one-temp)
      
      ;; Multiple variable binding
      (temps
       ("[" (separated-list optional-identifier ",") "]")
       many-temp)
      
      ;; Optional identifier for ignoring values in destructuring
      (optional-identifier
       ("_")
       ignore)

      ;; Optional identifier for binding values
      (optional-identifier
       (identifier)
       id) 
      
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  ;; Generate datatypes based on the grammar
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  ;; Function to display the defined datatypes
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  ;; Function to scan and parse input strings
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  ;; Function to only scan (tokenize) input strings
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )

#|
An example of how the data structures look from inside

(define-datatype program program?
  (a-program
   (a-program21 expression?)
   ))

(define-datatype expression expression?
  (const-exp
   (const-exp22 number?))
  (diff-exp
   (diff-exp23 expression?) (diff-exp24 expression?))
  (zero?-exp
   (zero?-exp25 expression?))
  (if-exp
   (if-exp26 expression?) (if-exp27 expression?) (if-exp28 expression?))
  (var-exp
   (var-exp29 symbol?))
  (let-exp
   (let-exp30 temps?) (let-exp31 expression?) (let-exp32 expression?))
  (proc-exp
   (proc-exp33 symbol?) (proc-exp34 expression?))
  (call-exp
   (call-exp35 expression?) (call-exp36 expression?))
  (tuple-exp
   (tuple-exp37 (list-of expression?))))

(define-datatype temps temps?
  (one-temp
   (one-temp38 symbol?))
  (many-temp
   (many-temp39 (list-of optional-identifier?))))

(define-datatype optional-identifier optional-identifier?
  (ignore)
  (id
   (id40 symbol?)))
|#