(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  (define test-list
    '(
      ; Example 1: Simple number test
      (test-1-simple-num-val2 "5" 5)

      ; Example 2: Arithmetic operation resulting in num-val
      (test-2-arith-num-val "-(10, 5)" 5)

      ; Example 3: Let expression with num-val
      (test-3-let-num-val "let x = 10 in -(x, 5)" 5)

      ; Example 4: If expression returning num-val
      (test-4-if-num-val "if zero?(-(5, 5)) then 10 else 20" 10)

      ; Example 5: If expression with arithmetic in branches
      (test-5-if-arith-num-val "if zero?(-(5, 5)) then -(10, 5) else -(20, 10)" 5)

      ; Example 6: try-catch-finally with num-val in try block
      (test-6-try-catch-num-val "try {
                if zero?(-(4,4)) then 10 else 20
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : 13 ; 
        catch [environment] : 14 ;
        finally : 200 ;" 10)

      ; Example 7: try-catch-finally with num-val in catch block
      (test-7-try-catch-num-val-in-catch "try {
                if zero?(-(4,6)) then 10 else 20
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : 13 ; 
        catch [environment] : 14 ;
        finally : 200 ;" 20)

      ; Example 8: try-catch-finally with num-val in finally block
      (test-8-try-finally-num-val "try {
                10
        }
        catch [general] : 11 ;
        finally : 200 ;" 10)
      
      ; Test cases follow the format: (name program-string expected-result)

      ; Example 9: Evaluating a let expression with an exception
      ; This should raise an exception because the expression "zero? (9)" is boolean, but used as a number.
      (test-9-let-test "let t=-(6, zero? (9)) in 78" (excp-val (Exception "not a number")))

      ; Example 10: Evaluating an expression with a non-existent variable in the environment
      ; This should raise an environment exception because 't' is not defined.
      (test-10-undefined-var-test "-(t, 8)" (excp-val (Exception "environment")))

      ; Example 11: Throwing an exception manually
      ; This should directly return a general exception.
      (test-11-throw-test "throw general" (excp-val (Exception "general")))

      ; Example 12: try with multiple catch and finally
      ; This should catch the "not a boolean" exception and return 13.
      (test-12-try-catch-test "try {
                -(if -(4,6) then 70 else 20, 45)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : 13 ; 
        catch [environment] : 14 ;
        finally : 200 ;" 13)

      ; Example 13: try with throwing an exception inside catch
      ; This should catch the "not a boolean" exception, then throw a general exception.
      (test-13-try-catch-throw-test "try {
                -(if -(4,6) then 70 else 20, 45)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : throw general ; 
        catch [environment] : 14 ;
        finally : 200 ;" (excp-val (Exception "general")))

      ; Example 14: try without handling the appropriate exception
      ; This should raise a "not a boolean" exception since there is no catch for this specific exception.
      (test-14-try-catch-missing-test "try {
                -(if -(4,6) then 70 else 20 , 45)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [environment] : 14 ;
        finally : 200 ;" (excp-val (Exception "not a boolean")))

      ; Simple constant evaluation
      (test-15-const-eval-test "11" 11)
      (test-16-const-eval-test-negative "-33" -33)

      ; Simple arithmetic evaluation
      (test-17-arith-eval-test-simple "-(44,33)" 11)
      (test-18-arith-eval-test-nested "-(-(44,33),22)" -11)
      (test-19-arith-eval-test-complex "-(55, -(22,11))" 44)

      ; Variable evaluation
      (test-20-var-eval-test-missing "r" (excp-val (Exception "environment")))
      (test-21-var-eval-test "let r = 3 in r" 3)
      (test-22-var-eval-test-complex "let r = -(4,1) in r" 3)

      ; Undefined variable
      (test-23-undefined-var-test-simple "foo" (excp-val (Exception "environment")))
      (test-24-undefined-var-test-complex "let x = 3 in y" (excp-val (Exception "environment")))

      ; If expressions
      (test-25-if-eval-test-true "if zero?(0) then 3 else 4" 3)
      (test-26-if-eval-test-false "if zero?(1) then 3 else 4" 4)
      (test-27-if-eval-test-exception "if 1 then 2 else 3" (excp-val (Exception "not a boolean")))

      ; Let expressions
      (test-28-let-eval-test-simple "let x = 3 in let y = 4 in -(x,y)" -1)
      (test-29-let-eval-test-redefine "let x = 3 in let x = 4 in x" 4)
      (test-30-let-eval-test-nested "let x = 3 in let x = -(x,1) in x" 2)
      
      ; Throw expressions
      (test-31-throw-eval-test-general "throw general" (excp-val (Exception "general")))
      (test-32-throw-eval-test-not-a-number "throw not a number" (excp-val (Exception "not a number")))

      ; Try expressions with various catch blocks
      (test-33-try-eval-test-catch "try { throw not a number } catch [not a number] : 99 ;" 99)
      (test-34-try-eval-test-catch-general "try { throw general } catch [not a number] : 99 ;" (excp-val (Exception "general")))
      (test-35-try-eval-test-catch-general-handled "try { throw general } catch [general] : 99 ;" 99)
      (test-36-try-eval-test-catch-mismatch "try { throw not a boolean } catch [not a number] : 99 ;" (excp-val (Exception "not a boolean")))
      (test-37-try-eval-test-catch-match "try { throw not a boolean } catch [not a boolean] : 99 ;" 99)

      ; Try expressions with finally block
      (test-38-try-finally-eval-test "try { throw not a number } catch [not a number] : 99 ; finally : 200 ;" 99)
      (test-39-try-finally-eval-test-no-exception "try { 1 } finally : 200 ;" (excp-val (Exception "general")))
      (test-40-try-finally-eval-test-throw-in-finally "try { 1 } finally : throw general ;" (excp-val (Exception "general")))

      ; Nested try expressions
      (test-41-nested-try-eval-test "try { try { throw not a number } catch [not a number] : throw general ; } catch [general] : 99 ;" 99)
      (test-42-nested-try-eval-test-general "try { try { throw general } catch [not a number] : 99 ; } catch [general] : 100 ;" 100)
      

      ;; Basic Tests
      (test-43-simple-num-val "5" 5)
      (test-44-negative-num-val "-5" -5)
      (test-45-let-num-val "let x = 10 in x" 10)

      ;; If Expressions
      (test-46-if-true "if zero?(0) then 1 else 2" 1)
      (test-47-if-false "if zero?(1) then 1 else 2" 2)

      ;; Arithmetic Operations
      (test-48-diff-simple "-(5, 3)" 2)
      (test-49-diff-negative "-(3, 5)" -2)

      ;; Let Expressions
      (test-50-let-simple "let x = 5 in -(x, 2)" 3)
      (test-51-let-nested "let x = 5 in let y = 3 in -(x, y)" 2)

      ;; Throw and Catch Expressions
      (test-52-try-catch "try { throw not a number } catch [not a number] : 11 ; finally : 200 ;" 11)
      (test-53-try-catch-wrong-exception "try { throw not a boolean } catch [not a number] : 11 ; finally : 200 ;" (excp-val (Exception "not a boolean")))
      (test-54-throw-no-catch "throw not a number" (excp-val (Exception "not a number")))
      (test-55-try-no-throw "try { -(11,10) } catch [not a number] : 11 ; finally : 200 ;" 1)
      (test-56-try-valid "try { let x = 5 in x } catch [not a number] : 11 ; finally : 200 ;" 5)

      ;; Complex Expressions
      (test-57-try-let-catch "try { let x = throw not a number in x } catch [not a number] : 11 ; finally : 200 ;" 11)
      (test-58-try-if-catch "try { if zero?(0) then throw not a number else 1 } catch [not a number] : 11 ; finally : 200 ;" 11)
      )
    
    ))
