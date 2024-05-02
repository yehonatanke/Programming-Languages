### question in page 87

```scheme
#lang racket
; matrix ADT spec
;-----------------

;---------------------------------------------------------------------------
;a. signatures

;constructors:
;  zeros       : int X int     -> matrix
;  mul-scalar  : int X matrix  -> matrix
;  insert      : matrix X int X int X int  --> matrix
;  add-mat     : matrix X matrix --> matrix
;
;observers:
;    rows      : matrix --> int
;    cols      : matrix --> int
;
;predicats:
;    is-I?     : matrix --> boolean
;-------------------------------------------------------------------------------  
; b. semantics

; (zeros m n )           =  Aij=0    where 0<i<m+1   0<j<n+1
; (mul-scalar num MAT )  =  Aij = num*MATij    for 0<i<(rows MAT)+1    0<j<(cols MAT)+1
; (insert MAT a b val)   =  Aij = val    where i=a and j=b
;                           Aij = MATij  otherwise
; (add-mat   MAT1 MAT2)  =  Aij = MAT1ij + MAT2ij   for 0<i<(rows MAT)+1  0<j<(cols MAT)+1  and MAT1 and MAT2 same dimentions
;                           error otherwise
; (rows (zeros m n ))    =  m
; (cols (zeros m n ))    =  n
; (is-I? MAT)            =  	
;                        =  false    otherwise


;--------------------------------------------------------------------------------
; c. implementation  
        
(define make-zero-row
      (lambda (n) 
         (if (= n 0) '() (cons 0 (make-zero-row (- n 1))))))

(define (zeros m n)
   (if (= m 0) '() (cons (make-zero-row n) (zeros (- m 1) n))))

;---------------------------------------------------------------------
(define (add-mat mat1 mat2)
     (map (lambda (l1 l2) (map + l1 l2)) mat1 mat2))
;---------------------------------------------------------------------

(define (rows mat) (length mat))

(define (cols mat) (length (car mat)))
;----------------------------------------------------------------------
(define (set-list-at lst i val)
   (if (null? lst)
       null
       (if (> i 1) 
           (cons (car lst) (set-list-at (cdr lst) (- i 1) val))
           (cons val (cdr lst)))))
       

(define (insert mat i j val)
  (if (and (<= i (rows mat)) (<= j (cols mat)))
      (if (> i 1)
          (cons (car mat) (insert (cdr mat) (- i 1) j val))
          (cons (set-list-at (car mat) j val) (cdr mat)))
      
      mat))  
;-----------------------------------------------------------------      
(define (process-matrix m func)
  (map (lambda (l)
         (map func l))
       m))
       
(define (mul-scalar num mat)
  (process-matrix mat (lambda (x) (* x num))))

;------------------------------------------------------------------
(define (is-I? mat)
  (letrec ((m (rows mat))
        (n (cols mat))
        (aij (lambda (mat i j) (list-ref (list-ref mat (- i 1)) (- j 1))))
        (valid-aij? (lambda (i j) (if (= i j) (= (aij mat i j) 1) (= (aij mat i j) 0))))
        (check-I? (lambda (i j ) (if (and (<= i m) (<= j n)) 
                                     (if (valid-aij? i j) 
                                         (if (< j n) 
                                             (check-I? i (+ j 1)) 
                                             (check-I? (+ i 1) 1))  
                                         #f) 
                                     #t))))
    (check-I? 1 1)))
        
                 
               
    
          
          
                      

