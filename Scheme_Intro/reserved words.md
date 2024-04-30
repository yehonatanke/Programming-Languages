# Reserved Words

1. [define](#define)
2. [lambda](#lambda)
3. [if](#if)
4. [else](#else)
5. [cond](#cond)
6. [let](#let)
7. [let*](#let*)
8. [letrec](#letrec)
9. [and](#and)
10. [or](#or)
11. [quote](#quote)
12. [set!](#set!)
13. [begin](#begin)
14. [do](#do)
15. [case](#case)

## Reserved Words - Extended

1. **define**: Defines a new variable or function.

    Example:
   
   ```(define x 10)```
   
   Input: None
   
   Output: Value of `x` is now `10`.

2. **lambda**: Creates an anonymous function.
 
   Example:

    ```((lambda (x) (* x x)) 5)```

   Input: `5`

   Output: `25`

3. **if**: Conditional statement.

   Example:

   ```(if (> x 0) "positive" "non-positive")```

   Input: `x = 5`

   Output: `"positive"`

4. **else**: Part of a conditional statement to handle alternative cases.

   Example:

   ```(if (> x 0) "positive" (if (= x 0) "zero" "negative"))```

   Input: `x = -3`

   Output: `"negative"`

5. **cond**: Multi-branch conditional statement.

   Example:

   ```(cond ((= x 0) "zero") ((> x 0) "positive") (else "negative"))```

   Input: `x = 10`

   Output: `"positive"`

6. **let**: Binds variables to values locally.

   Example:

   ```(let ((x 10) (y 20)) (+ x y))```
   
   Input: None
   
   Output: `30`

7. **let***: Similar to let, but variables are defined sequentially.

   Example:

   ```(let* ((x 10) (y (+ x 5))) y)```
   
   Input: None
   
   Output: `15`

8. **letrec**: Defines local recursive functions.

   Example:
   
   ```(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))```
   
   Input: None
   
   Output: `120`

9. **and**: Logical AND operator.

   Example:
   
   ```(and (> x 5) (< x 10))```
   
   Input: `x = 7`
   
   Output: `#t`

10. **or**: Logical OR operator.

     Example:

    ```(or (= x 0) (= x 1))```

    Input: `x = 0`

    Output: `#t`

11. **quote**: Prevents evaluation of an expression.

    Example:

    ```(quote (+ 1 2))```

    Input: None

    Output: `(+ 1 2)`

12. **set!**: Modifies the value of a variable.

    Example:

    ```(set! x 20)```

    Input: None

    Output: Value of `x` is now `20`.

13. **begin**: Evaluates a sequence of expressions, returning the value of the last one.

    Example:

    ```(begin (display "Hello, ") (display "world!"))```

    Input: None

    Output: `Hello, world!`

14. **do**: Used for iteration.

    Example:

    ```(do ((i 0 (+ i 1))) ((= i 5) 'done) (display i))```

    Input: None

    Output: `01234`

15. **case**: A variant of cond for multi-branch conditionals.

    Example:

    ```(case x ((1) "one") ((2) "two") (else "other"))```

    Input: `x = 2`

    Output: `"two"`
