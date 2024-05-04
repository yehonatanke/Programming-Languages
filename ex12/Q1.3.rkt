#lang eopl

;Q1.3

(define pos-int?
 (lambda (n)
   (and(integer? n) (> n 0))))
 
; Define the Matrix data type
(define-datatype matrix matrix?
  (zeros (rows pos-int?) (cols pos-int?))
  (mul-scalar (num number?) (matrix matrix?))
  (insert (matrix matrix?) (row pos-int?) (col pos-int?) (val number?)))

; Predicate that checks if both matrices are of the same size and are valid matrices
(define is-matrix-same-size?
  (lambda (mat1)
    (lambda (mat2)
      (if (and (and (and (= (cols mat1) (cols mat2))
                         (= (rows mat1) (rows mat2)))
                    (matrix? mat1))
               (matrix? mat2))
          #t
          (eopl:error "Error: Matrices should be of equal size")))))

; add-mat: Accepts two matrices and returns their sum
(define add-mat
  (lambda (mat1 mat2)
    (cond
      ((is-matrix-same-size? mat1 mat2)
       (let ((rows (rows mat1))
             (cols (cols mat1)))
         (let loop ((r 0) (c 0) (result (zeros rows cols)))
           (if (= r rows)
               result
               (if (= c cols)
                   (loop (+ r 1) 0 result)
                   (loop r (+ c 1)
                         (insert result r c
                                 (+ (elem mat1 r c) (elem mat2 r c)))))))))
      (else (eopl:error "Error: Matrices should be of equal size")))))

; elem: Helper function to get the element at a given row and column
(define elem
  (lambda (mat r c)
    (cases matrix mat
      (zeros (rows cols) 0)
      (mul-scalar (num m) (* num (elem m r c)))
      (insert (m row col val) (if (and (= r row) (= c col))
                                  val
                                  (elem m r c))))))

; rows: Accepts a matrix and returns the number of rows
(define rows
  (lambda (mat)
    (cases matrix mat
      (zeros (r c) r)
      (mul-scalar (n m) (rows m))
      (insert (m r c v) (rows m)))))

; cols: Accepts a matrix and returns the number of columns
(define cols
  (lambda (mat)
    (cases matrix mat
      (zeros (r c) c)
      (mul-scalar (n m) (cols m))
      (insert (m r c v) (cols m)))))

; is-I?: Accepts a matrix and returns #t if it represents the identity matrix
(define is-I?
  (lambda (mat)
    (let ((rows (rows mat))
          (cols (cols mat)))
      (cond
        ((not (= rows cols)) #f) ; Identity matrix must be square
        ((zero? rows) #t) ; Empty matrix is considered an identity matrix
        (else
         (let loop ((r 0) (c 0))
           (cond
             ((= r rows) #t) ; Reached the end, all diagonal elements are 1
             ((= c cols) (loop (+ r 1) 0)) ; Move to the next row
             ((and (= r c) (= 1 (elem mat r c))) (loop r (+ c 1))) ; Diagonal element is 1
             ((and (not (= r c)) (= 0 (elem mat r c))) (loop r (+ c 1))) ; Non-diagonal element is 0
             (else #f)))) ; Invalid element
        ))))

; print-mat: Accepts a matrix and prints it nicely
(define print-mat
  (lambda (mat)
    (let ((rows (rows mat))
          (cols (cols mat)))
      (display "[ ")
      (let loop ((r 0) (c 0))
        (cond
          ((= r rows)
           (display "]"))
          ((= c cols)
           (display "]\n[ ")
           (loop (+ r 1) 0))
          (else
           (display (elem mat r c))
           (display " ")
           (loop r (+ c 1))))))))

