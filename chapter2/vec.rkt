; Vector ADT Implementation 

#lang eopl
 
(define mycheck?
  (lambda  (x)
      (and (integer? x) (positive? x))))


(define-datatype vec vec?
  (zeros 
    (n  mycheck?))
  (insert 
     (v vec?)
     (idx mycheck?)
     (val number?))
  )


(define (makezero n)
       (if (= 0 n) 
           '()
         (cons 0 (makezero (- n 1)))))


(define print-vec
  (lambda (vc) 
    (cases vec vc 
      (zeros (n)  (makezero n))
      (insert (v idx val)  (let ((vlist (print-vec v)))
                             (helper vlist idx val))))))

(define (helper vlist idx val)
   (if (= idx 1) 
       (cons val (cdr vlist))
       (cons (car vlist ) (helper (cdr vlist) (- idx 1) val))))
                                  
                                 
    


        
        
        
        
        
        
