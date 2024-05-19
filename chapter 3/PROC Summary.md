# <p align="center"> Summary of PROC in Scheme </p>

#### Table of Contents
1. [Introduction](#introduction)
2. [Syntax](#syntax)
3. [Definitions](#definitions)
4. [Semantics](#semantics)
5. [Examples](#examples)
6. [Advantages](#advantages)
7. [Common Uses](#common-uses)

---

### Introduction
The `proc` (short for "procedure") in Scheme refers to the creation and use of procedures, which are functions that can take arguments and perform computations. Procedures are first-class citizens in Scheme, meaning they can be passed as arguments, returned from other procedures, and assigned to variables.

### Syntax
The basic syntax for defining a procedure in Scheme is as follows:

```scheme
(define (procedure-name arg1 arg2 ...)
  body)
```
- `procedure-name` is the name of the procedure.
- `arg1`, `arg2`, ... are the arguments the procedure takes.
- `body` is the sequence of expressions that define the procedure's behavior.

### Definitions
- **Procedure:** A sequence of expressions that performs a specific task, defined using the `define` keyword.
- **First-Class Citizen:** An entity that can be passed as an argument, returned from a function, and assigned to a variable.

### Expression Syntax
Here is the formal syntax definition for procedures in Scheme:

```
ProcedureDefinition ::= (define (ProcedureName Arg1 Arg2 ...) Body)
ProcedureName ::= Identifier
Arg ::= Identifier
Body ::= Expression
Expression ::= Atom | List | ProcedureCall
Atom ::= Number | Symbol | String | Boolean
List ::= '(' Expression* ')'
ProcedureCall ::= '(' ProcedureName Expression* ')'
```

### Semantics
The semantics of procedures in Scheme are as follows:

1. **Procedure Definition:**
   - When a procedure is defined using `define`, the procedure name is associated with a lambda expression in the environment.
   - The lambda expression captures the argument list and the body of the procedure.

2. **Procedure Application:**
   - When a procedure is called, the arguments are evaluated in the current environment.
   - A new environment is created, extending the current environment, where the procedure's arguments are bound to the evaluated values.
   - The body of the procedure is evaluated in this new environment.
   - The result of the last expression in the body is returned as the value of the procedure call.

3. **First-Class Procedures:**
   - Procedures can be passed as arguments to other procedures.
   - Procedures can be returned as values from other procedures.
   - Procedures can be assigned to variables, enabling higher-order programming.

### Examples
Here are some examples to illustrate the use of procedures:

1. **Simple Procedure:**
   ```scheme
   (define (add x y)
     (+ x y))
   (add 3 4)  ; Returns 7
   ```

2. **Procedure with Conditional Logic:**
   ```scheme
   (define (absolute-value x)
     (if (< x 0)
         (- x)
         x))
   (absolute-value -5)  ; Returns 5
   ```

3. **Higher-Order Procedure:**
   ```scheme
   (define (apply-twice f x)
     (f (f x)))
   (define (square x)
     (* x x))
   (apply-twice square 2)  ; Returns 16
   ```

### Advantages
- **Abstraction:** Procedures allow for abstraction and code reuse.
- **Modularity:** Breaking down complex problems into simpler, reusable procedures.
- **First-Class Procedures:** Procedures can be manipulated just like any other data.

### Common Uses
- **Mathematical Computations:** Defining reusable mathematical functions.
- **Control Structures:** Creating custom control structures.
- **Higher-Order Functions:** Functions that take other functions as arguments or return them as results, enabling powerful abstraction patterns.
