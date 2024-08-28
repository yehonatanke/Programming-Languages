# Summary of Scheme Built-in Functions

This list includes common built-in functions in Scheme.

## Table of Contents
1. [Arithmetic Functions](#arithmetic-functions)
2. [Comparison Functions](#comparison-functions)
3. [Logical Functions](#logical-functions)
4. [Number Type Checking](#number-type-checking)
5. [List Operations](#list-operations)
6. [String Operations](#string-operations)
7. [Symbol Operations](#symbol-operations)
8. [Input/Output](#inputoutput)
9. [Control Functions](#control-functions)
10. [System Functions](#system-functions)

## Arithmetic Functions

### Basic Operations

- `+`: Addition
  - Example: 
    ```scheme
    (+ 2 3 4)
    ```
  - Output: `9`
  - Explanation: Adds all provided numbers.

- `-`: Subtraction
  - Example:
    ```scheme
    (- 10 3 2)
    ```
  - Output: `5`
  - Explanation: Subtracts all numbers from the first one.

- `*`: Multiplication
  - Example:
    ```scheme
    (* 2 3 4)
    ```
  - Output: `24`
  - Explanation: Multiplies all provided numbers.

- `/`: Division
  - Example:
    ```scheme
    (/ 20 5 2)
    ```
  - Output: `2`
  - Explanation: Divides the first number by all subsequent numbers.

### Advanced Operations

- `quotient`: Integer division
  - Example:
    ```scheme
    (quotient 17 5)
    ```
  - Output: `3`
  - Explanation: Returns the integer quotient.

- `remainder`: Remainder of division
  - Example:
    ```scheme
    (remainder 17 5)
    ```
  - Output: `2`
  - Explanation: Returns the remainder of integer division.

- `modulo`: Modulus operation
  - Example:
    ```scheme
    (modulo -13 5)
    ```
  - Output: `2`
  - Explanation: Returns the modulus (similar to remainder but handles negative numbers differently).

- `abs`: Absolute value
  - Example:
    ```scheme
    (abs -7)
    ```
  - Output: `7`
  - Explanation: Returns the absolute value of a number.

- `max`: Maximum value
  - Example:
    ```scheme
    (max 3 7 2 10 5)
    ```
  - Output: `10`
  - Explanation: Returns the largest of the given numbers.

- `min`: Minimum value
  - Example:
    ```scheme
    (min 3 7 2 10 5)
    ```
  - Output: `2`
  - Explanation: Returns the smallest of the given numbers.

- `sqrt`: Square root
  - Example:
    ```scheme
    (sqrt 16)
    ```
  - Output: `4`
  - Explanation: Returns the square root of a number.

- `expt`: Exponentiation
  - Example:
    ```scheme
    (expt 2 3)
    ```
  - Output: `8`
  - Explanation: Raises the first number to the power of the second.

## Comparison Functions

### Numeric Comparisons

- `=`: Equality
  - Example:
    ```scheme
    (= 5 5 5)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if all arguments are equal.

- `<`: Less than
  - Example:
    ```scheme
    (< 3 5 7)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if each argument is less than the next.

- `>`: Greater than
  - Example:
    ```scheme
    (> 7 5 3)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if each argument is greater than the next.

- `<=`: Less than or equal to
  - Example:
    ```scheme
    (<= 3 3 5)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if each argument is less than or equal to the next.

- `>=`: Greater than or equal to
  - Example:
    ```scheme
    (>= 5 5 3)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if each argument is greater than or equal to the next.

### General Comparisons

- `eq?`: Identity comparison
  - Example:
    ```scheme
    (eq? 'a 'a)
    ```
  - Output: `#t` (true)
  - Explanation: Tests if two objects are the same object in memory.

- `eqv?`: Equivalence comparison
  - Example:
    ```scheme
    (eqv? 2 2)
    ```
  - Output: `#t` (true)
  - Explanation: Similar to `eq?` but also works for numbers and characters.

- `equal?`: Structural equality
  - Example:
    ```scheme
    (equal? '(1 2 3) '(1 2 3))
    ```
  - Output: `#t` (true)
  - Explanation: Tests if two objects have the same structure and contents.

## Logical Functions

- `and`: Logical AND
  - Example:
    ```scheme
    (and #t #f #t)
    ```
  - Output: `#f` (false)
  - Explanation: Returns true if all arguments are true, otherwise false.

- `or`: Logical OR
  - Example:
    ```scheme
    (or #f #t #f)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if any argument is true, otherwise false.

- `not`: Logical NOT
  - Example:
    ```scheme
    (not #f)
    ```
  - Output: `#t` (true)
  - Explanation: Returns the logical negation of its argument.

## Number Type Checking

- `number?`: Check if argument is a number
  - Example:
    ```scheme
    (number? 42)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is any kind of number.

- `complex?`: Check if argument is a complex number
  - Example:
    ```scheme
    (complex? 3+4i)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is a complex number.

- `real?`: Check if argument is a real number
  - Example:
    ```scheme
    (real? 3.14)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is a real number.

- `rational?`: Check if argument is a rational number
  - Example:
    ```scheme
    (rational? 1/3)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is a rational number.

- `integer?`: Check if argument is an integer
  - Example:
    ```scheme
    (integer? 42)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is an integer.

- `even?`: Check if argument is even
  - Example:
    ```scheme
    (even? 4)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is an even integer.

- `odd?`: Check if argument is odd
  - Example:
    ```scheme
    (odd? 3)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is an odd integer.

- `zero?`: Check if argument is zero
  - Example:
    ```scheme
    (zero? 0)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is zero.

- `positive?`: Check if argument is positive
  - Example:
    ```scheme
    (positive? 5)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is greater than zero.

- `negative?`: Check if argument is negative
  - Example:
    ```scheme
    (negative? -3)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is less than zero.

## List Operations

### Construction

- `cons`: Construct pair
  - Example:
    ```scheme
    (cons 1 '(2 3))
    ```
  - Output: `(1 2 3)`
  - Explanation: Creates a new pair with the first argument as the car and the second as the cdr.

- `list`: Create list
  - Example:
    ```scheme
    (list 1 2 3)
    ```
  - Output: `(1 2 3)`
  - Explanation: Creates a new list containing the given arguments.

### Access

- `car`: First element of list
  - Example:
    ```scheme
    (car '(1 2 3))
    ```
  - Output: `1`
  - Explanation: Returns the first element of a list.

- `cdr`: Rest of list
  - Example:
    ```scheme
    (cdr '(1 2 3))
    ```
  - Output: `(2 3)`
  - Explanation: Returns the rest of the list after the first element.

### Utility

- `list?`: Check if argument is a list
  - Example:
    ```scheme
    (list? '(1 2 3))
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is a proper list.

- `length`: Length of list
  - Example:
    ```scheme
    (length '(1 2 3))
    ```
  - Output: `3`
  - Explanation: Returns the number of elements in the list.

- `append`: Concatenate lists
  - Example:
    ```scheme
    (append '(1 2) '(3 4))
    ```
  - Output: `(1 2 3 4)`
  - Explanation: Joins two or more lists together.

- `reverse`: Reverse list
  - Example:
    ```scheme
    (reverse '(1 2 3))
    ```
  - Output: `(3 2 1)`
  - Explanation: Returns a new list with elements in reverse order.

- `member`: Check if element is in list
  - Example:
    ```scheme
    (member 2 '(1 2 3))
    ```
  - Output: `(2 3)`
  - Explanation: Returns the sublist starting with the found element, or #f if not found.

- `assoc`: Association list lookup
  - Example:
    ```scheme
    (assoc 'b '((a 1) (b 2) (c 3)))
    ```
  - Output: `(b 2)`
  - Explanation: Looks up a key in an association list and returns the matching pair.

## String Operations

- `string?`: Check if argument is a string
  - Example:
    ```scheme
    (string? "hello")
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is a string.

- `string-length`: Length of string
  - Example:
    ```scheme
    (string-length "hello")
    ```
  - Output: `5`
  - Explanation: Returns the number of characters in the string.

- `string-ref`: Character at index
  - Example:
    ```scheme
    (string-ref "hello" 1)
    ```
  - Output: `#\e`
  - Explanation: Returns the character at the specified index (0-based).

- `string-append`: Concatenate strings
  - Example:
    ```scheme
    (string-append "hello" " " "world")
    ```
  - Output: `"hello world"`
  - Explanation: Joins two or more strings together.

- `substring`: Extract part of string
  - Example:
    ```scheme
    (substring "hello" 1 4)
    ```
  - Output: `"ell"`
  - Explanation: Returns a substring from start index (inclusive) to end index (exclusive).

## Symbol Operations

- `symbol?`: Check if argument is a symbol
  - Example:
    ```scheme
    (symbol? 'abc)
    ```
  - Output: `#t` (true)
  - Explanation: Returns true if the argument is a symbol.

- `symbol->string`: Convert symbol to string
  - Example:
    ```scheme
    (symbol->string 'abc)
    ```
  - Output: `"abc"`
  - Explanation: Converts a symbol to its string representation.

- `string->symbol`: Convert string to symbol
  - Example:
    ```scheme
    (string->symbol "abc")
    ```
  - Output: `abc`
  - Explanation: Converts a string to a symbol.

## Input/Output

- `display`: Print object
  - Example:
    ```scheme
    (display "Hello, world!")
    ```
  - Output: Prints "Hello, world!" to the console
  - Explanation: Prints the given object to the current output port.

- `write`: Print object (with quotation for strings)
  - Example:
    ```scheme
    (write "Hello, world!")
    ```
  - Output: Prints "Hello, world!" (with quotation marks) to the console
  - Explanation: Similar to display, but prints strings with quotation marks.

- `newline`: Print newline
  - Example:
    ```scheme
    (newline)
    ```
  - Output: Prints a newline character
  - Explanation: Moves to the next line on the current output port.

- `read`: Read S-expression
  - Example:
    ```scheme
    (read)
    ```
  - Input: User types `(+ 2 3)`
  - Output: `5`
  - Explanation: Reads an S-expression from the current input port and evaluates it.

- `read-char`: Read character
  - Example:
    ```scheme
    (read-char)
    ```
  - Input: User types 'a'
  - Output: `#\a`
  - Explanation: Reads a single character from the current input port.

- `peek-char`: Peek at next character
  - Example:
    ```scheme
    (peek-char)
    ```
  - Input: Next character is 'a'
  - Output: `#\a`
  - Explanation: Returns the next character without removing it from the input stream.

## Control Functions

- `cond`: Conditional expression
  - Example:
    ```scheme
    (define (describe-number x)
      (cond
        ((< x 0) "negative")
        ((= x 0) "zero")
        ((< x 10) "small positive")
        (else "large positive")))
    
    (describe-number 5)
    ```
  - Output: `"small positive"`
  - Explanation: Evaluates a series of test clauses and returns the value of the first clause whose test is true. The `else` clause is optional and acts as a catch-all.

- `apply`: Apply function to list
  - Example:
    ```scheme
    (apply + '(1 2 3 4))
    ```
  - Output: `10`
  - Explanation: Applies the given function to the elements of the list.

- `map`: Apply function to each element
  - Example:
    ```scheme
    (map (lambda (x) (* x x)) '(1 2 3))
    ```
  - Output: `(1 4 9)`
  - Explanation: Applies the given function to each element of the list and returns a new list.

- `for-each`: Apply function for side effects
  - Example:
    ```scheme
    (for-each display '(1 2 3))
    ```
  - Output: Prints "123" to the console
  - Explanation: Applies the given function to each element of the list for its side effects.

## System Functions

- `load`: Load and evaluate file
  - Example:
    ```scheme
    (load "myfile.scm")
    ```
  - Output: Evaluates the contents of "myfile.scm"
  - Explanation: Loads and evaluates the contents of the specified file.

- `eval`: Evaluate S-expression
  - Example:
    ```scheme
    (eval '(+ 1 2))
    ```
  - Output: `3`
  - Explanation: Evaluates the given S-expression.

- `error`: Raise an error
  - Example:
    ```scheme
    (error "Something went wrong")
    ```
  - Output: Raises an error with the given message
  - Explanation: Signals an error with the provided error message.

