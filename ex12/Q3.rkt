#lang racket

;Q3


;Define an empty environment with no bindings
(define empty-env
   (lambda()
     (list
      (lambda (search-var)
       (error "No binding found: " search-var)) 
      (lambda (search-var) #f)))) 

;Extend an existing environment with a new binding
(define extend-env
   (lambda (saved-var saved-val saved-env)
     (list
      (lambda (search-var)
       (if (eqv? search-var saved-var) ; Check if the variable matches the saved variable
           saved-val ; If true, return the saved value
           (apply-env saved-env search-var))) ; If false, recursively search in the saved environment
       (lambda (search-var)
         (if (eqv? search-var saved-var) ; Check if the variable matches the saved variable
             #t ; If true, return true (indicating the variable has a binding)
             ((cadr saved-env) search-var)))))) ; If false, recursively check in the saved environment

;Apply an environment to a variable to retrieve its value
(define apply-env
   (lambda (env search-var)
     ((car env) search-var))) 

;Check if a variable has a binding in the environment
(define (has-binding? env search-var)
   ((cadr env) search-var)) 

;Count the number of bindings for a given variable in the environment
(define (count-binding env search-var)
  ;Helper function for recursive counting
  (define (count-helper env search-var count)
    (cond
      ((null? env) count) ; Base case: reached end of environment
      ((eqv? search-var (car env)) ; Check if variable matches current binding
       (count-helper (cdr env) search-var (+ count 1))) ; Increment count if found
      (else
       (count-helper (cdr env) search-var count)))) ; Continue searching
  (count-helper env search-var 0)) ; Start with count 0 and call helper function

