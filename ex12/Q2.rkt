#lang eopl

(require "unittests.scm")

;EX2 - Q2


(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

;parse-prefix
(define (parse-prefix ls)
  (define (parser-prefix ls)
  (cond
    [(integer? (car ls)) (cons (const-exp (car ls)) (cdr ls))]
    [(equal? '- (car ls)) (begin
     (define operand1 (parser-prefix (cdr ls)))
     (define operand2 (parser-prefix (cdr operand1)))
     (cons (diff-exp (car operand1) (car operand2)) (cdr operand2)))]
    [else (eopl:error "Invalid Prefix Expression")]))
  (if (list? ls)
  (car (parser-prefix ls))
  (eopl:error "parse-prefix invalid input")))


;Unit Tests
(equal?? (parse-prefix '(-3 2)) (diff-exp (const-exp -3) (const-exp 2)))
(report-unit-tests-completed 'parse-prefix)