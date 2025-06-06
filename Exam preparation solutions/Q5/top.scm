(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list
  
  (provide run run-all)
  
  ;;;; function for automated testing ;;;;
  (provide test-all)
  (define (test-all) (run-all))


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
 
  ;; (run-all)

  ; Q5
  ; (run "let x = fold proc (y) -(y,-1) 0 [12, -(4,7), 9] in -(x,5)")

  #|
  (proc-val
   (procedure 'y
              (diff-exp (var-exp 'y) (const-exp -1))
              (list (list 'i (num-val 1)) (list 'v (num-val 5)) (list 'x (num-val 10)))
              ))
  interp.scm:86:58: call-exp: bad value for call-exp15 field: (proc-val (procedure 'y (diff-exp (var-exp 'y) (const-exp -1)) (list (list 'i (num-val 1)) (list 'v (num-val 5)) (list 'x (num-val 10)))))
  |#
  )




