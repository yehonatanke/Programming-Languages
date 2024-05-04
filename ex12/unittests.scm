(module utilts
  (lib "eopl.ss" "eopl")

(provide equal?? report-unit-tests-completed)

(define-syntax equal??
  (syntax-rules ()
    ((_ x y)
     (let ((x^ x)(y^ y))
       (if (not (equal? x y))
           (eopl:error 'equal??
                       "~s is not equal to ~s" 'x 'y))))))

(define report-unit-tests-completed
  (lambda (fn-name)
    (eopl:printf "unit tests completed: ~s~%" fn-name)))
  
)