(module tests mzscheme
  
  (provide test-list recursive-test-list Q2.1-tests)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

  ;; Q1.3 - Recursive Implementation for PROC
  (define recursive-test-list
    '(
      ;; recursive sum-up-to-n test

      ;; Check the recursive sum of numbers from 1 to n
      (sum-up-to-n-test-1 "let p = proc(n)
         if zero?(n) then 0
          else - (n, -(0,(p -(n, 1))))
         in (p 2)" 3)

      (sum-up-to-n-test-1 "let p = proc(n)
         if zero?(n) then 0
          else - (n, -(0,(p -(n, 1))))
         in (p 3)" 6)

      (sum-up-to-n-test-1 "let p = proc(n)
         if zero?(n) then 0
          else - (n, -(0,(p -(n, 1))))
         in (p 4)" 10)

      (sum-up-to-n-test-1 "let p = proc(n)
         if zero?(n) then 0
          else - (n, -(0,(p -(n, 1))))
         in (p 5)" 15)

      (sum-up-to-n-test-1 "let p = proc(n)
         if zero?(n) then 0
          else - (n, -(0,(p -(n, 1))))
         in (p 10)" 55)
      ))

  ;; Q2.1
  (define Q2.1-tests
    '(
      ;; Test with equal values
      (equal-values-test "let x = 5 in let y = 5 in let f = proc(x)
           proc(y)
           if zero?(-(x,y)) then zero?(-(x,y))
           else if zero?(-(-(x,0),y)) then -(-(0,x),y)
           else zero?(-(-(0,x),y))
          in ((f x) y)" #t)

      ;; Test with non-equal and non-opposite values
      (non-equal-non-opposite-values-test "let x = 3 in let y = 4 in let f = proc(x)
                                             proc(y)
                                             if zero?(-(x,y)) then zero?(-(x,y))
                                             else if zero?(-(-(x,0),y)) then -(-(0,x),y)
                                             else zero?(-(-(0,x),y))
                                           in ((f x) y)" #f)

      ;; Test with opposite values
      (opposite-values-test "let x = 6 in let y = -6 in let f = proc(x)
                              proc(y)
                              if zero?(-(x,y)) then zero?(-(x,y))
                              else if zero?(-(-(x,0),y)) then -(-(0,x),y)
                              else zero?(-(-(0,x),y))
                            in ((f x) y)" #t)

      ;; Test with zero values
      (zero-values-test "let x = 0 in let y = 0 in let f = proc(x)
                          proc(y)
                          if zero?(-(x,y)) then zero?(-(x,y))
                          else if zero?(-(-(x,0),y)) then -(-(0,x),y)
                          else zero?(-(-(0,x),y))
                        in ((f x) y)" #t)

      ;; Test with different positive and negative values
      (different-positive-negative-test "let x = 5 in let y = -5 in let f = proc(x)
                                          proc(y)
                                          if zero?(-(x,y)) then zero?(-(x,y))
                                          else if zero?(-(-(x,0),y)) then -(-(0,x),y)
                                          else zero?(-(-(0,x),y))
                                        in ((f x) y)" #t)

      ;; Test with different negative values
      (different-negative-values-test "let x = -3 in let y = -4 in let f = proc(x)
                                         proc(y)
                                         if zero?(-(x,y)) then zero?(-(x,y))
                                         else if zero?(-(-(x,0),y)) then -(-(0,x),y)
                                         else zero?(-(-(0,x),y))
                                       in ((f x) y)" #f)

      ;; Test with positive and zero values
      (positive-zero-values-test "let x = 5 in let y = 0 in let f = proc(x)
                                    proc(y)
                                    if zero?(-(x,y)) then zero?(-(x,y))
                                    else if zero?(-(-(x,0),y)) then -(-(0,x),y)
                                    else zero?(-(-(0,x),y))
                                  in ((f x) y)" #f)
      )
    )
  
  (define test-list1
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
                     -1)
      
      (y-combinator-1 "
       let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
          in let
          t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
         in let times4 = (fix t4m)
         in (times4 3)" 12)
      ))

  ;; dynamic binding tests
  (define test-list ;test-list-dynamic-binding
    '(
      ;; Check that the value of 'a' in the procedure is 5, not 3
      (dynamic-binding-1 "let a = 3
                          in let p = proc(x)
                          - (x, a) in let a = 5
                          in - (a, (p 2))" 8)

      ;; Check two procedures with dynamic binding
      (dynamic-binding-2 "
        let a = 10 in 
        let b = 20 in 
        let p1 = proc(x) - (x, a) in 
        let p2 = proc(x) - (x, b) in 
        let a = 30 in 
        let b = 40 in 
        - ((p1 5), (p2 5))" 10)

      ;; Check a procedure within an if expression with dynamic binding
      (dynamic-binding-3 "let a = 1 in let p = proc(x) if zero?(x) then a else - (x, a) in let a = 2 in (p 0)" 2)

      ;; Check nested expressions with a procedure and dynamic binding
      (dynamic-binding-4 "
        let a = 3 in 
        let b = 4 in 
        let p = proc(x) - (x, a) in 
        let a = 5 in 
        let b = 6 in 
        - ((p b), a)" -4)

      ;; Check a procedure with a changing value during the call
      (dynamic-binding-5 "
        let a = 2 in 
        let b = 3 in 
        let p = proc(x) - (x, a) in 
        let a = 4 in 
        (p b)" -1)

      (dynamic-binding-6 "let a = 3 in let p = proc(x) - (x, a) in let a = 5 in (p 2)" -3)
      ))

  ;; simple tests
  (define test-list-simple
    '(
      ;; dynamic binding test example
      ;; Check the difference between lexical and dynamic binding
      (dynamic-vs-lexical-binding "let a = 3 in let p = proc(x) - (x, a) in let a = 5 in (p 2)" -3)
      ))
  )