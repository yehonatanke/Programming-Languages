```scheme
; Vector ADT Implementation with define-datatype

#lang eopl ; Specify the language as Extended Scheme (EOPL)

(define mycheck? ; Define a helper function to check if a value is a positive integer
  (lambda  (x)
      (and (integer? x) (positive? x))))

(define-datatype vec vec? ; Define a vector data type using define-datatype
  (zeros  ; Constructor for creating a vector of zeros
    (n  mycheck?))  ; Number of elements in the vector
  (insert  ; Constructor for inserting a value at a specific index in the vector
     (v vec?)  ; Original vector
     (idx mycheck?)  ; Index where the value will be inserted
     (val number?))  ; Value to be inserted
  )

(define (makezero n) ; Define a function to create a vector of zeros of length n
       (if (= 0 n) 
           '()  ; Base case: return an empty list if n is zero
         (cons 0 (makezero (- n 1)))))  ; Recursive case: prepend 0 and recursively call makezero with n-1

(define print-vec  ; Define a function to print the vector
  (lambda (vc) 
    (cases vec vc  ; Pattern match the vector cases
      (zeros (n)  (makezero n))  ; If the vector is zeros, create a vector of zeros
      (insert (v idx val)  ; If the vector is an insertion, extract components
              (let ((vlist (print-vec v)))  ; Recursively print the original vector
                (helper vlist idx val))))))  ; Call helper function to insert the value at the specified index

(define (helper vlist idx val)  ; Define a helper function to insert a value at a specific index
   (if (= idx 1)  ; If the index is 1
       (cons val (cdr vlist))  ; Insert the value at the beginning of the list
       (cons (car vlist ) (helper (cdr vlist) (- idx 1) val))))  ; Otherwise, recursively call helper with updated index and value

