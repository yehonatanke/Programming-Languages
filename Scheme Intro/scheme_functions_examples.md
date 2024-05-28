# <p align="center"> Scheme Functions Examples </p>
 
# Scheme Functions Table

| Function Name | Description                             | Example                                      |
|---------------|-----------------------------------------|----------------------------------------------|
| `define`      | Defines a new variable or function.     | [Example](#define-example)                   |
| `lambda`      | Creates an anonymous function.          | [Example](#lambda-example)                   |
| `if`          | Conditional expression.                 | [Example](#if-example)                       |
| `cond`        | Multi-way conditional expression.       | [Example](#cond-example)                     |
| `car`         | Returns the first element of a list.    | [Example](#car-example)                      |
| `cdr`         | Returns the rest of the list.           | [Example](#cdr-example)                      |
| `cons`        | Constructs a new pair from two values.  | [Example](#cons-example)                     |
| `list`        | Creates a list from given elements.     | [Example](#list-example)                     |
| `map`         | Applies a function to each element of a list. | [Example](#map-example)              |
| `filter`      | Filters elements of a list based on a predicate. | [Example](#filter-example)          |
| `let`         | Binds variables to values for a local scope. | [Example](#let-example)             |
| `let*`        | Binds variables sequentially.           | [Example](#let*-example)                     |
| `begin`       | Groups multiple expressions.            | [Example](#begin-example)                    |
| `set!`        | Changes the value of an existing variable. | [Example](#set-example)                    |
| `eq?`         | Tests for object identity.               | [Example](#eq-example)                       |
| `equal?`      | Tests for structural equality.           | [Example](#equal-example)                    |
| `apply`       | Applies a function to a list of arguments. | [Example](#apply-example)                 |
| `append`      | Concatenates lists.                      | [Example](#append-example)                   |
| `length`      | Returns the length of a list.            | [Example](#length-example)                   |
| `reverse`     | Reverses a list.                         | [Example](#reverse-example)                  |
| `memq`        | Searches for an element in a list.       | [Example](#memq-example)                     |

## Examples

### `define` Example

```scheme
(define x 10)
(define (square n)
  (* n n))
```

### `lambda` Example

```scheme
(define square (lambda (n) (* n n)))
```

### `if` Example

```scheme
(if (> 3 2)
    'yes
    'no)
```

### `cond` Example

```scheme
(cond
  ((> 3 2) 'greater)
  ((< 3 2) 'less)
  (else 'equal))
```

### `car` Example

```scheme
(car '(1 2 3)) ; returns 1
```

### `cdr` Example

```scheme
(cdr '(1 2 3)) ; returns (2 3)
```

### `cons` Example

```scheme
(cons 1 '(2 3)) ; returns (1 2 3)
```

### `list` Example

```scheme
(list 1 2 3 4) ; returns (1 2 3 4)
```

### `map` Example

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4)) ; returns (1 4 9 16)
```

### `filter` Example

```scheme
(filter (lambda (x) (> x 2)) '(1 2 3 4)) ; returns (3 4)
```

### `let` Example

```scheme
(let ((x 2)
      (y 3))
  (+ x y)) ; returns 5
```

### `let*` Example

```scheme
(let* ((x 2)
       (y (+ x 3)))
  (* x y)) ; returns 10
```

### `begin` Example

```scheme
(begin
  (display "Hello")
  (newline)
  (display "World")) ; prints "Hello" and then "World"
```

### `set!` Example

```scheme
(define x 10)
(set! x 20)
x ; returns 20
```

### `eq?` Example

```scheme
(eq? 'a 'a) ; returns #t
(eq? '(1 2 3) '(1 2 3)) ; returns #f
```

### `equal?` Example

```scheme
(equal? '(1 2 3) '(1 2 3)) ; returns #t
```

### `apply` Example

```scheme
(apply + '(1 2 3 4)) ; returns 10
```

### `append` Example

```scheme
(append '(1 2) '(3 4)) ; returns (1 2 3 4)
```

### `length` Example

```scheme
(length '(1 2 3 4)) ; returns 4
```

### `reverse` Example

```scheme
(reverse '(1 2 3 4)) ; returns (4 3 2 1)
```

### `memq` Example

```scheme
(memq 'b '(a b c)) ; returns (b c)
```
```

This updated file includes a broader range of Scheme functions along with their examples.
