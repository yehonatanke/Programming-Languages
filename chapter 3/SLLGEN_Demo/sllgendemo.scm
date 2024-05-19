#lang eopl


;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
(define the-lexical-spec
'((white-sp (whitespace) skip)
(comment ("%" (arbno (not #\newline))) skip)
(identifier (letter (arbno (or letter digit))) symbol)
(number (digit (arbno digit)) number)
(number ("-" digit (arbno digit)) number)))

(define the-grammar
'((statement
("{" statement ";" statement "}")
compound-statement)
(statement
("while" expression "do" statement)
while-statement)
(statement
   (identifier ":=" expression)
 assign-statement)
(expression
   (identifier)
var-exp)

(expression
   (number)
num-exp)

(expression
("(" expression "-" expression ")")
diff-exp)))

 
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
 

 ;(scan&parse "{x := foo; %dhgfhdghfgd
              ;  while x do              x := (x - bar)}")

 


