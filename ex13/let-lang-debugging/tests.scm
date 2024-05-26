(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list2
    '(
      ; Example 1: Simple number test
      (simple-num-val "5" ( 5))
      (simple-num-val2 "5" (num-val 5))

      ; Example 2: Arithmetic operation resulting in num-val
      (arith-num-val "-(10, 5)" (num-val 5))

      ; Example 3: Let expression with num-val
      (let-num-val "let x = 10 in -(x, 5)" (num-val 5))

      ; Example 4: If expression returning num-val
      (if-num-val "if zero?(-(5, 5)) then 10 else 20" (num-val 10))

      ; Example 5: If expression with arithmetic in branches
      (if-arith-num-val "if zero?(-(5, 5)) then -(10, 5) else -(20, 10)" (num-val 5))

      ; Example 6: try-catch-finally with num-val in try block
      (try-catch-num-val "try {
                -(if zero?(-(4,4)) then 10 else 20)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : 13 ; 
        catch [environment] : 14 ;
        finally : 200 ;
" (num-val 10))

      ; Example 7: try-catch-finally with num-val in catch block
      (try-catch-num-val-in-catch "try {
                -(if zero?(-(4,6)) then 10 else 20)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : 13 ; 
        catch [environment] : 14 ;
        finally : 200 ;
" (num-val 13))

      ; Example 8: try-catch-finally with num-val in finally block
      (try-finally-num-val "try {
                10
        }
        catch [general] : 11 ;
        finally : 200 ;
" (num-val 10))
      ))
  


  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      ; Test cases follow the format: (name program-string expected-result)

      ; Example 1: Evaluating a let expression with an exception
      ; This should raise an exception because the expression "zero? (9)" is boolean, but used as a number.
      (let-test "let t=-(6, zero? (9)) in 78" (excp-val (Exception "not a number")))

      ; Example 2: Evaluating an expression with a non-existent variable in the environment
      ; This should raise an environment exception because 't' is not defined.
      (undefined-var-test "-(t, 8)" (excp-val (Exception "environment")))

      ; Example 3: Throwing an exception manually
      ; This should directly return a general exception.
      (throw-test "throw general" (excp-val (Exception "general")))

      ; Example 4: try with multiple catch and finally
      ; This should catch the "not a boolean" exception and return 13.
      (try-catch-test "try {
                -(if -(4,6) then 70 else 20, 45)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : 13 ; 
        catch [environment] : 14 ;
        finally : 200 ;"
                      13)

      ; Example 5: try with throwing an exception inside catch
      ; This should catch the "not a boolean" exception, then throw a general exception.
      (try-catch-throw-test "try {
                -(if -(4,6) then 70 else 20, 45)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [not a boolean] : throw general ; 
        catch [environment] : 14 ;
        finally : 200 ;"
                            (excp-val (Exception "general")))

      ; Example 6: try without handling the appropriate exception
      ; This should raise a "not a boolean" exception since there is no catch for this specific exception.
      (try-catch-missing-test "try {
                -(if -(4,6) then 70 else 20 , 45)
        }
        catch [general] : 11 ;
        catch [not a number] : 12 ; 
        catch [environment] : 14 ;
        finally : 200 ;"
                              (excp-val (Exception "not a boolean")))

      ;;;;;
            ; Simple constant evaluation
      (const-eval-test "11" 11)
      (const-eval-test "-33" -33)

      ; Simple arithmetic evaluation
      (arith-eval-test "-(44,33)" 11)
      (arith-eval-test "-(-(44,33),22)" -11)
      (arith-eval-test "-(55, -(22,11))" 44)

      ; Variable evaluation
      (var-eval-test "x" '(excp-val (Exception "environment")))
      (var-eval-test "let x = 3 in x" 3)
      (var-eval-test "let x = -(4,1) in x" 3)

      ; Undefined variable
      (undefined-var-test "foo" '(excp-val (Exception "environment")))
      (undefined-var-test "let x = 3 in y" '(excp-val (Exception "environment")))

      ; If expressions
      (if-eval-test "if zero?(0) then 3 else 4" 3)
      (if-eval-test "if zero?(1) then 3 else 4" 4)
      (if-eval-test "if 1 then 2 else 3" '(excp-val (Exception "not a boolean")))

      ; Let expressions
      (let-eval-test "let x = 3 in let y = 4 in -(x,y)" -1)
      (let-eval-test "let x = 3 in let x = 4 in x" 4)
      (let-eval-test "let x = 3 in let x = -(x,1) in x" 2)
      
      ; Throw expressions
      (throw-eval-test "throw general" '(excp-val (Exception "general")))
      (throw-eval-test "throw not a number" '(excp-val (Exception "not a number")))

      ; Try expressions with various catch blocks
      (try-eval-test "try { throw not a number } catch [not a number] : 99 ;" 99)
      (try-eval-test "try { throw general } catch [not a number] : 99 ;" '(excp-val (Exception "general")))
      (try-eval-test "try { throw general } catch [general] : 99 ;" 99)
      (try-eval-test "try { throw not a boolean } catch [not a number] : 99 ;" '(excp-val (Exception "not a boolean")))
      (try-eval-test "try { throw not a boolean } catch [not a boolean] : 99 ;" 99)

      ; Try expressions with finally block
      (try-finally-eval-test "try { throw not a number } catch [not a number] : 99 ; finally : 200 ;" 99)
      (try-finally-eval-test "try { 1 } finally : 200 ;" 1)
      (try-finally-eval-test "try { 1 } finally : throw general ;" '(excp-val (Exception "general")))

      ; Nested try expressions
      (nested-try-eval-test "try { try { throw not a number } catch [not a number] : throw general ; } catch [general] : 99 ;" 99)
      (nested-try-eval-test "try { try { throw general } catch [not a number] : 99 ; } catch [general] : 100 ;" 100)

      ))
  )
