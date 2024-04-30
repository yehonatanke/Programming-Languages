### Key Concepts in Programming Language Theory

1. **Identifiers**: Names used to identify variables, functions, and other entities in a program.
   - *Example*: `x`, `calculateArea`

2. **Terminals**: Symbols that appear in the input of a language and cannot be further decomposed (literals and identifiers).
   - *Example*: Keywords like `if`, `else`, punctuation marks like `;`, `,`

3. **Non-terminals**: Symbols in a formal grammar that represent groups of terminals or other non-terminals.
   - *Example*: `<expression>`, `<statement>`

4. **Production Rules**: Rules describing how symbols can be replaced by other symbols in a formal grammar.
   - *Example*: 
     ```
     <statement> ::= <if-statement> | <assignment>
     <if-statement> ::= if (<condition>) <block> [else <block>]
     ```

5. **Syntax**: The set of rules defining the structure of valid sentences or expressions in a programming language.
   - *Example*: In many programming languages, the syntax for an if statement is `if (condition) { statement } [else { statement }]`.

6. **Semantics**: The meaning associated with the syntactic structures of a programming language.
   - *Example*: In the expression `x = 5`, the semantics dictate that the value `5` should be assigned to the variable `x`.

7. **Tokens**: The smallest units of meaning in a programming language, such as keywords, identifiers, literals, and operators.
   - *Example*: In the expression `x + 2`, the tokens are `x`, `+`, and `2`.

8. **Parsing**: The process of analyzing a sequence of tokens to determine its syntactic structure according to a formal grammar.
   - *Example*: Parsing the expression `x = 5 + 2` would involve recognizing the assignment operation and arithmetic addition.

9. **Abstract Syntax Tree (AST)**: A tree representation of the syntactic structure of a program.
   - *Example*: For the expression `x = 5 + 2`, the AST would have a root node representing the assignment, with child nodes representing the variable `x` and the addition operation.

10. **Context-free Grammar (CFG)**: A formal grammar in which the left-hand side of each production rule consists of a single non-terminal symbol.
    - *Example*: A CFG for arithmetic expressions might have rules like `<expression> ::= <term> | <expression> + <term>`.

11. **Lexical Analysis**: The process of breaking input into a sequence of tokens.
    - *Example*: Lexical analysis of the expression `x = 5 + 2` would identify tokens like `x`, `=`, `5`, `+`, and `2`.

12. **Regular Expressions**: Patterns used to describe sets of strings.
    - *Example*: A regular expression for matching integers might be `\d+`.

13. **Finite Automata**: Mathematical models of computation used in lexical analysis to recognize patterns defined by regular expressions.
    - *Example*: A finite automaton can be designed to recognize whether a given string of characters represents a valid integer.

14. **Backus-Naur Form (BNF)**: A notation used to describe the syntax of programming languages and other formal languages.
    - *Example*:
      ```
      <statement> ::= <if-statement> | <assignment>
      <if-statement> ::= if (<condition>) <block> [else <block>]
      ```

15. **Type Systems**: Formal systems used to classify and reason about the types of data used in a program.
    - *Example*: In a statically typed language, the type system would ensure that operations like adding an integer to a string are not allowed.
