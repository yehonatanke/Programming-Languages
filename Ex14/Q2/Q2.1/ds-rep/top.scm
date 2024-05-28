(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list and recursive-test-list
  
  (provide run run-all run-all-recursive)
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))
  
  ;; run-all : () -> Unspecified
  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  
  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
    
  ;; run-one : Sym -> ExpVal
  ;; (run-one sym) runs the test whose name is sym
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))


  ;; run-all-recursive : () -> Unspecified
  ;; runs all the tests in recursive-test-list, comparing the results with
  ;; equal-answer?  
  (define run-all-recursive
    (lambda ()
      (run-tests! run equal-answer? recursive-test-list)))


  (define run-Q2.1-tests
    (lambda ()
      (run-tests! run equal-answer? Q2.1-tests)))


  ; To run Q1.1 run "(run-all)"
  ;; (run-all)

  ;; To run the recursive calls for Q1.3 run "(run-all-recursive)"
  ;; (run-all-recursive)

  ;; To run the tests for Q2.1 run "(run-Q2.1-tests)"
  ;; (run-Q2.1-tests) 



  ; (run "let makemult = proc (maker) proc (x)
  ; if zero? (x)
  ; then 0
  ; else -(((maker maker) -(x,1)) , -4)
  ; in let times4 = proc (x) ((makemult makemult) x) in (times4 3)")

  ;(run " let a=10 in let p=proc(w)- (-(w,2),a) in (p 3)") --> (num-val -9)
  )




