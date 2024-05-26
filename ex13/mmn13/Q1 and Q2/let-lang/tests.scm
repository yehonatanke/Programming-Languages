(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
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

      ;; Multiplication Tests
      (mul-positive "*(3, 4)" 12)
      (mul-negative "*(3, -4)" -12)
      (mul-zero "*(3, 0)" 0)

      ;; Addition Tests
      (add-positive "+(3, 4)" 7)
      (add-negative "+(3, -4)" -1)
      (add-zero "+(3, 0)" 3)

      ;; Division Tests
      (div-positive "/(12, 4)" 3)
      (div-negative "/(12, -4)" -3)
      (div-by-one "/(12, 1)" 12)
      (div-by-negative-one "/(12, -1)" -12)

      ;; List and Cons Tests
      (test-list-cons-car "let x = 4 in car(cons(x, cons(cons(-(x,1), emptylist), emptylist)))" 4)
      (test-list-cons-cdr-car "let x = 4 in car(car(cdr(cons(x, cons(cons(-(x,1), emptylist), emptylist)))))" 3)
      (test-list-cons-cdr-cdr-null "let x = 4 in null?(cdr(cdr(cons(x, cons(cons(-(x,1), emptylist), emptylist)))))" #t)
      (test-list-cons-cdr-cdr-non-null "let x = 4 in null?(car(cdr(cons(x, cons(cons(-(x,1), emptylist), emptylist)))))" #f)

      ;; List Tests
      (test-list-elements-second "let x = 4 in car(cdr(list(x, -(x,1), -(x,3))))" 3)
      (test-list-elements-third "let x = 4 in car(cdr(cdr(list(x, -(x,1), -(x,3)))))" 1)
      (test-list-elements-null "let x = 4 in null?(cdr(cdr(cdr(list(x, -(x,1), -(x,3))))))" #t)
      
      ;; Equality Tests
      (equal-test-1 "equal?(1,2)"  #f)
      (equal-test-2 "equal?(2,1)"  #f)
      (equal-test-3 "equal?(1,1)"  #t)

      ;; Less Than Tests
      (less-test-1 "less?(1,0)"  #f)
      (less-test-2 "less?(1,1)"  #f)
      (less-test-3 "less?(3,4)"  #t)

      ;; Greater Than Tests
      (greater-test-1 "greater?(0,1)"  #f)
      (greater-test-2 "greater?(1,1)"  #f)
      (greater-test-3 "greater?(2,1)"  #t)
     
      ))
  )