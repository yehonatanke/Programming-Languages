```scheme
#lang eopl

(require "utils.scm")

(define-datatype stack stack?
  (empty-stack)
  (push
   (old-stack stack?)
   (v number?)))

(define error
  (lambda (op)
    (eopl:error op "Cannot ~s empty stack" op)))

(define pop
  (lambda (stck)
    (cases stack stck 
      (empty-stack () (error 'pop))
      (push (prev-stack v) prev-stack))))

(define top
  (lambda (stck)
    (cases stack stck
      (empty-stack () (error 'top))
      (push (prev-stack v) v))))

(define empty-stack?
  (lambda (stck)
    (cases stack stck
      (empty-stack () #t)
      (else #f))))
      ;(push (prev-stack v) #f))))

(define show-stack-as-list
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())
      (push (prev-stack v) (cons v (show-stack-as-list prev-stack))))))


;; Test Examples:
;; ---------------------------------
(define stack1 (empty-stack))
(define stack2 (push stack1 5))
(define stack3 (push stack2 3))
(define stack4 (push stack3 8))

; working with unit tests
(equal?? (empty-stack? stack1) #t )
(equal?? (empty-stack? stack3) #f)
(equal?? (top stack4) 8)
(equal?? (pop stack4) stack3)
(equal?? (show-stack-as-list (pop stack4)) '(3 5))
(equal?? (show-stack-as-list stack4) '(8 3 5))
(equal?? (show-stack-as-list stack1) '())
(report-unit-tests-completed )


; working without unit tests
;(eopl:printf "\n ~s" (empty-stack? stack1) )
;(eopl:printf "\n ~s" (empty-stack? stack3))
;(eopl:printf "\n ~s" (top stack4))
;(eopl:printf "\n ~s" (pop stack4))
;(eopl:printf "\n ~s" (show-stack-as-list (pop stack4)))
;(eopl:printf "\n ~s" (show-stack-as-list stack4))
;(eopl:printf "\n ~s" (show-stack-as-list stack1))
; (top stack1))
; (pop stack1))
