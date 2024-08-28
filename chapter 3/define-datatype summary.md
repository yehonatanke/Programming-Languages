# define-datatype 

`define-datatype` is a macro used in some Scheme implementations, particularly those focused on teaching programming language concepts. It's commonly used in the book "Essentials of Programming Languages" (EOPL).

## Purpose
- Defines algebraic data types
- Used to create structured data with variants or cases
- Particularly useful for representing abstract syntax trees in language implementation

## Syntax

## Syntax

```scheme
(define-datatype type-name type-predicate
  (variant1 (field1 predicate1)
            (field2 predicate2)
            ...)
  (variant2 (field1 predicate1)
            (field2 predicate2)
            ...)
  ...)
```

## Components
- `type-name`: Name of the new data type
- `type-predicate`: Automatically generated function to check if a value is of this type
- `variant`: Name of each variant (or case) of the data type
- `field`: Components of each variant
- `predicate`: Function to validate each field's value

## Example 01

```scheme
(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?)))
```

## Examples 02

### Basic Example

```scheme
(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?)))
```
This code is defining a new data type called `expval`, which stands for "expressed value". It's commonly used in interpreters to represent the result of evaluating an expression. Let's break it down:

1. `expval`: This is the name of the new data type being defined.

2. `expval?`: This is the name of the predicate function that will be automatically generated. It will return true for any value that is an `expval`.

3. The data type has two variants:

   a. `num-val`:
      - This variant represents numeric values.
      - It has one field called `value`.
      - The `number?` predicate indicates that this field must be a number.

   b. `bool-val`:
      - This variant represents boolean values.
      - It has one field called `boolean`.
      - The `boolean?` predicate indicates that this field must be a boolean.

This definition allows you to create and work with `expval` values in two ways:

1. You can create numeric expressed values:
   ```scheme
   (num-val 5)  ; represents the number 5 as an expval
   ```

2. You can create boolean expressed values:
   ```scheme
   (bool-val #t)  ; represents the boolean true as an expval
   ```

The purpose of this datatype is to provide a way to represent the results of evaluating expressions in a simple language that only has numbers and booleans. It's a common pattern in interpreters to have a distinct type for "expressed values" that's separate from the language's abstract syntax.

This definition also implicitly creates constructor functions (`num-val` and `bool-val`) and accessor functions (often named something like `expval->num` and `expval->bool`, though these are not shown in the definition).


## Features
- Automatically generates constructor and accessor functions
- Enables pattern matching with a `cases` special form
- Facilitates creation of interpreters and compilers
- Useful for modeling domain-specific languages

## Use Cases
- Implementing interpreters
- Representing programming language constructs
- Modeling structured data in functional programming

