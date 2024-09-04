(module lang (lib "eopl.ss" "eopl")                
  
  ;; language for IMPLICIT-REFS

  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  ; Define the lexical specification which identifies the tokens for the language
  (define the-lexical-spec
    '(
      (whitespace (whitespace)
                  skip)   ; Whitespace is skipped

      (comment ("%" (arbno (not #\newline)))
               skip)  ; Comments starting with '%' and running to end of line are skipped

      (identifier (letter (arbno (or letter digit "_" "-" "?")))
                  symbol) ; Identifiers made of letters, digits, '_', '-', '?' are treated as symbols

      (number (digit (arbno digit))
              number)  ; A sequence of digits is a number
      
      (number ("-" digit (arbno digit))
              number)  ; Negative numbers are also recognized
      ))

  
  ; Define the grammar for the language, associating expressions with specific AST node types
  (define the-grammar
    '((program (expression) a-program)  ; A program consists of a single expression

      ;; Base expression types
      (expression (number) const-exp)  ; Constant expression (a number)
      (expression
       ("-" "(" expression "," expression ")")  ; Subtraction of two expressions
       diff-exp)
    
      (expression
       ("zero?" "(" expression ")")  ; Check if an expression is zero
       zero?-exp)

      ;; Conditional expression
      (expression
       ("if" expression "then" expression "else" expression)  ; If-then-else expression
       if-exp)

      (expression (identifier) var-exp)  ; Variable expression (an identifier)

      ;; Let expression for defining local variables
      (expression
       ("let" identifier "=" expression "in" expression)  
       let-exp)   

      ;; Procedure expression for defining anonymous functions
      ; (expression
      ; ("proc" "(" identifier ")" expression)
      ; proc-exp)

      ;; Function call expression
      (expression
       ("(" expression expression ")")
       call-exp)

      ;; Recursive let expression for defining mutually recursive functions
      (expression
       ("letrec"
        (arbno identifier "(" identifier ")" "=" expression)
        "in" expression)
       letrec-exp)
    
      ;; Block expression with multiple expressions
      (expression
       ("begin" expression (arbno ";" expression) "end")
       begin-exp)

      ;; New for implicit references
      ;; Assignment expression for setting a variable's value
      (expression
       ("set" identifier "=" expression)
       assign-exp)

      ;; solution for Q1 - new procedure definition
      (expression
       ("proc" "(" identifier ")" (arbno "static" identifier "=" expression) expression)
       proc-exp)
    
      ))

    
  
  

  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
