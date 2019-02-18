# Overview
This the project is a custom programming language created as a group for the [UNO](https://www.unomaha.edu/) course [CSCI 4220](https://catalog.unomaha.edu/search/?search=CSCI%204220).

# Project Overview
In this course, we are required to do a project. This will allow us to further develop our understanding of the material covered by applying it in a hands-on fashion. We formed a group consisting of four members. Each of us is responsible for understanding all aspects of the project, and the instructor reserves the right to test us on this material in class. The project will be broken down into [4 Milestones](#milestones), each with an associated set of deliverables.

## Milestones

### Milestone 1
The first milestone concerns itself with defining the syntax of the language and can be accomplished by developing a context-free grammar for the language as well as a specification of the tokens that are used by that grammar. In the first milestone, we should also explicitly state the associativity and precedence of each operator in the grammar.
- Inputs to Milestone 1
  - The [Language Overview](#language-overview)
- Expected Outputs
  - A BNF description of the language.
  - A regular expression description of the tokens in the language.
  - A document describing the precedence and associativity of the operators in the language.

### Milestone 2
In the second milestone, we will provide a formal denotational description of the semantics of the language. An organized way to do this is to first define a model of computation (an abstract computer) and to then describe in general terms what it means to "execute" each construct in the language.
- Inputs to Milestone 2
  - The outputs of [Milestone 1](#milestone-1).
- Expected Outputs
  - A denotational description of the semantics of the language written in the style presented in class.

### Milestone 3
In the third milestone, we will construct a TL domain for the language. The domain will allow us to parse programs in the language, check for the presence of ambiguously defined constructs, view the parse tree corresponding to programs, and output the parse tree to a file. How this can be accomplished is discussed in class. In this phase, we are also required to (1) translate a small Java program that the instructor gives us into an equivalent program in the language, and (2) verify that this program can be correctly parsed.
- Inputs to Milestone 3
  - The outputs of [Milestone 1](#milestone-1).
  - A sample program, which the instructor will provide.
  - A basic (sample) TL domain, which the instructor will provide.
- Expected Outputs
  - A zipped TL domain containing the files necessary to parse programs in the language (remove the bin directory from the TL domain).
  - The sample program translated into the syntax of the language.

### Milestone 4
In the fourth milestone, we will write a denotational-semantic based interpreter for the language. This interpreter must be written in ML and integrated with the TL system.
- Inputs to Milestone 4
  - A template providing a basic interpreter architecture that has been integrated with the TL system.
  - The syntax folder developed in [Milestone 3](#milestone-3).
- Expected Outputs
  - A TL domain in which the interpreter for the language is integrated.
  - A set of programs in the language demonstrating that the interpreter works. This set must contain the language’s version of the `three_x_plus_1.java` program that was provide in milestone three.

# Language Overview
This project involves the design of a small imperative programming language containing the following constructs: [Statements](#statements) and [Expressions](#expressions).

## Statements
- Declaration statements
- Assignment statements
- Conditional statements
  - if-then-else statements must be supported
  - if-then statements must be supported (be careful of the dangling else problem).
- Pre/post increment/decrement statements (e.g., + +x;x−−; etc.)
- Iterative statements
  - while-loops must be supported
  - for-loops must be supported
- Block statements
- Print statements – a statement that prints the value of an expression

## Expressions
The language should support the full range of expression involving the operations shown below. Note that this syntax will need to include parenthesis.
- Types:  Boolean and Integer
- Operations:

  | Description    | Operator | Signature                                 |
  |----------------|----------|-------------------------------------------|
  | conjunction    | and      | boolean ∗ boolean → boolean               |
  | disjunction    | or       | boolean ∗ boolean → boolean               |
  | negation       | not      | boolean → boolean                         |
  | unary minus    | -        | integer → integer                         |
  |                |          |                                           |
  | post-increment | ++       | integer ref → integer (has a side-effect) |
  | pre-increment  | ++       | integer ref → integer (has a side-effect) |
  | post-decrement | --       | integer ref → integer (has a side-effect) |
  | pre-decrement  | --       | integer ref → integer (has a side-effect) |
  |                |          |                                           |
  | add            | +        | integer ∗ integer → integer               |
  | subtract       | -        | integer ∗ integer → integer               |
  | multiply       | *        | integer ∗ integer → integer               |
  | integer divide | div      | integer ∗ integer → integer               |
  | modulus        | mod      | integer ∗ integer → integer               |
  | exponent       | ^        | integer ∗ integer → integer               |
  | abs            | \|\|     | integer → integer                         |
  | less-than      | <        | integer ∗ integer → integer               |
  | greater-than   | >        | integer ∗ integer → integer               |
  | equal-to       | =        | any ∗ any → boolean                       |
  | not equal-to   | !=       | any ∗ any → boolean                       |
