```scheme
#lang eopl

(require "utils.scm") ; Importing utility functions

(define-datatype stack stack? ; Defining a stack data type
  (empty-stack)              ; Constructor for an empty stack
  (push                      ; Constructor for pushing a value onto the stack
   (old-stack stack?)       ; Previous stack
   (v number?)))            ; Value to be pushed onto the stack

(define error  ; Define error function to handle stack errors
  (lambda (op)
    (eopl:error op "Cannot ~s empty stack" op)))

(define pop  ; Define pop function to remove the top element from the stack
  (lambda (stck)
    (cases stack stck
      (empty-stack () (error 'pop))  ; If the stack is empty, raise an error
      (push (prev-stack v) prev-stack))))  ; Otherwise, return the previous stack

(define top  ; Define top function to return the top element of the stack
  (lambda (stck)
    (cases stack stck
      (empty-stack () (error 'top))  ; If the stack is empty, raise an error
      (push (prev-stack v) v))))  ; Otherwise, return the value of the top element

(define empty-stack?  ; Define empty-stack? function to check if the stack is empty
  (lambda (stck)
    (cases stack stck
      (empty-stack () #t)  ; If the stack is empty, return true
      (else #f))))  ; Otherwise, return false

(define show-stack-as-list  ; Define show-stack-as-list function to display the stack as a list
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())  ; If the stack is empty, return an empty list
      (push (prev-stack v) (cons v (show-stack-as-list prev-stack))))))  ; Otherwise, return a list with the top element followed by the rest of the stack as a list

;; Test Examples:
;; ---------------------------------
(define stack1 (empty-stack))  ; Create an empty stack
(define stack2 (push stack1 5))  ; Push 5 onto the stack
(define stack3 (push stack2 3))  ; Push 3 onto the stack
(define stack4 (push stack3 8))  ; Push 8 onto the stack

; working with unit tests
(equal?? (empty-stack? stack1) #t )  ; Test if stack1 is empty
(equal?? (empty-stack? stack3) #f)  ; Test if stack3 is empty
(equal?? (top stack4) 8)  ; Test the top element of stack4
(equal?? (pop stack4) stack3)  ; Test popping an element from stack4
(equal?? (show-stack-as-list (pop stack4)) '(3 5))  ; Test showing the stack as a list after popping an element
(equal?? (show-stack-as-list stack4) '(8 3 5))  ; Test showing the stack as a list
(equal?? (show-stack-as-list stack1) '())  ; Test showing an empty stack as a list
(report-unit-tests-completed )  ; Report that unit tests have been completed

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
