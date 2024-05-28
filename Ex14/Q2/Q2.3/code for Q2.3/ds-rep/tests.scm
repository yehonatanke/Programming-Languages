(module tests mzscheme
  
  (provide test-list Q2.3-is-power-of-2 Q2.2-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

  (define Q2.2-list
    '(

      Q2.2 "let makemult = proc (maker) proc (x)
            if zero? (x)
            then 0
            else -(((maker maker) -(x,1)) , -4)
      in let times4 = proc (x) ((makemult makemult) x)
         in (times4 3)" 12)
    )

  (define Q2.3-is-power-of-2
    '(
      ;; Test 01: 9 is not a power of 2
      (test-01 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 9)
" #f)

      ;; Test 02: 1 is a power of 2 (2^0)
      (test-02 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 1)
" #t)

      ;; Test 03: 2 is a power of 2 (2^1)
      (test-03 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 2)
" #t)

      ;; Test 04: 4 is a power of 2 (2^2)
      (test-04 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 4)
" #t)

      ;; Test 05: 8 is a power of 2 (2^3)
      (test-05 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 8)
" #t)

      ;; Test 06: 16 is a power of 2 (2^4)
      (test-06 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 16)
" #t)

      ;; Test 07: 32 is a power of 2 (2^5)
      (test-07 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 32)
" #t)

      ;; Test 08: 64 is a power of 2 (2^6)
      (test-08 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 64)
" #t)

      ;; Test 09: 100 is not a power of 2
      (test-09 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
    in if zero? (-(x, -(next, -(0, next))))
      then ((helper helper) next)
      else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 100)
" #f)

       ;; Test 10: 4096 is a power of 2 (2^12)
      (test-10 "
let half-helper = proc (helper) proc (x)
  if zero? (x)
  then 0
  else if zero? (-(x, 1))
  then 0
  else -(((helper helper) -(x, 2)), -1)
in let half = proc (x) ((half-helper half-helper) x)
in let is-power-of-2-helper = proc (helper) proc (x)
  if zero? (x)
  then zero?(1)
  else if zero? (-(x, 1))
  then zero?(0) 
  else let next = (half x)
   in if zero? (-(x, -(next, -(0,next))))
    then ((helper helper) next)
    else zero?(1)
in let is-power-of-2 = proc (x) ((is-power-of-2-helper is-power-of-2-helper) x)
in (is-power-of-2 4096)
" #t)
      ))

  
  (define divide-by-2-list
    '(

      divide-by-2 "let divide-by-2-helper = proc (helper) proc (x)
                           if zero? (x)
                           then 0
                           else if zero? (-(x, 1))
                           then 0
                           else -(((helper helper) -(x, 2)), -1)
     in let divide-by-2 = proc (x) ((divide-by-2-helper divide-by-2-helper) x)
     in (divide-by-2 16)" 8)
    )
  
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
  )