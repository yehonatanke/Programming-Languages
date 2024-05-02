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
16. [lang](#lang)

# Reserved Words - Extended

## define 

**Description:** Defines a new variable or function.

**Structure:** Variable and Function Definition

**Example:**
   
```scheme
(define x 10)
```
   
**Input:** None
   
**Output:** Value of `x` is now `10`.

## lambda 

**Description:** Creates an anonymous function.

**Structure:** Anonymous Function Creation
 
**Example:**

```scheme
; ((lambda (x) (* x x)) 5)

((lambda (x)
   (* x x))
 5)
```

**Input:** `5`

**Output:** `25`

## if 

**Description:** Conditional statement.

**Structure:** Conditional Statement

**Example:**

```scheme
; (if (> x 0) "positive" "non-positive")

(if (> x 0)
   "positive"
   ; else
   "non-positive")
```
   
**Input:** `x = 5`

**Output:** `"positive"`

## else 

**Description:** Part of a conditional statement to handle alternative cases.

**Structure:** Alternative Case Handling

**Example:**

```scheme
; (if (> x 0) "positive" (if (= x 0) "zero" "negative"))

(if (> x 0)
    "positive"
    (if (= x 0)
        "zero"
        "negative"))

```

**Input:** `x = -3`

**Output:** `"negative"`

## cond 

**Description:** Multi-branch conditional statement.

**Structure:** Multi-branch Conditional Statement

**Example:**

```scheme
; (cond ((= x 0) "zero") ((> x 0) "positive") (else "negative"))

(cond 
  ((= x 0) "zero") 
  ((> x 0) "positive") 
  (else "negative"))
```

**Input:** `x = 10`

**Output:** `"positive"`

## let 

**Description:**  Binds variables to values locally.

**Structure:** Local Variable Binding

**Example:**

```scheme
; (let ((x 10) (y 20)) (+ x y))

(let ((x 10) (y 20))
  (+ x y))
```
   
**Input:** None
   
**Output:** `30`

## let* 

**Description:**  Similar to let, but variables are defined sequentially.

**Structure:** Sequential Variable Binding

**Example:**

```scheme
; (let* ((x 10) (y (+ x 5))) y)

(let* ((x 10)
       (y (+ x 5)))
  y)
```

**Input:** None
   
**Output:** `15`

## letrec 

**Description:**  Defines local recursive functions.

**Structure:** Local Recursive Function Definition

**Example:**
   
```scheme
; (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))

(letrec ((fact (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1)))))))
  (fact 5))

```
   
**Input:** None
   
**Output:** `120`

## and 

**Description:**  Logical AND operator.

**Structure:** Logical AND Operator

**Example:**
   
```scheme
; (and (> x 5) (< x 10))

(and (> x 5)
     (< x 10))
```

**Input:** `x = 7`
   
**Output:** `#t`

## or 

**Description:**  Logical OR operator.

**Structure:** Logical OR Operator

**Example:**

```scheme
; (or (= x 0) (= x 1))

(or (= x 0)
    (= x 1))
```
    
**Input:** `x = 0`

**Output:** `#t`

## quote 

**Description:**  Prevents evaluation of an expression.

**Structure:** Expression Prevention

**Example:**

```scheme
(quote (+ 1 2))
```

**Input:** None

**Output:** `(+ 1 2)`

## set!

**Description:**  Modifies the value of a variable.

**Structure:** Variable Value Modification

**Example:**

```scheme
(set! x 20)
```

**Input:** None

**Output:** Value of `x` is now `20`.

## begin 

**Description:**  Evaluates a sequence of expressions, returning the value of the last one.

**Structure:** Expression Sequence Evaluation

**Example:**

```scheme
; (begin (display "Hello, ") (display "world!"))

(begin (display "Hello, ")
       (display "world!"))
```

**Input:** None

**Output:** `Hello, world!`

## do 

**Description:**  Used for iteration.

**Structure:** Iteration Control

**Example:**

```scheme
; (do ((i 0 (+ i 1))) ((= i 5) 'done) (display i))

(do ((i 0 (+ i 1)))
    ((= i 5) 'done)
  (display i))
```

**Input:** None

**Output:** `01234`

## case 

**Description:**  A variant of cond for multi-branch conditionals.

**Structure:** Multi-branch Conditional Variant

**Example:**

```scheme
; (case x ((1) "one") ((2) "two") (else "other"))

(case x ((1) "one")
        ((2) "two")
  (else "other"))
```

**Input:** `x = 2`

**Output:** `"two"`

## lang

**Description:** Specifies the specific language or language variant being used in a Scheme program.

**Structure:** Language Specification

**Example:**

```scheme
#lang eopl
```

**Input:** None

**Output:** Interprets the code using the EoPL (Essentials of Programming Languages) language variant.
