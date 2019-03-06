# Associative and Precedence

| Precedence  | Description              | Operator | Signature                   | Associativity   |
|:-----------:|--------------------------|:--------:|-----------------------------|-----------------|
| 9 (Highest) | Unary Logical NOT        | not      | boolean → boolean           | Right to Left   |
| 8           | Exponent                 | ^        | integer * integer → integer | Right to Left   |
| 7           | Unary Minus              | -        | integer → integer           | Right to Left   |
| 6           | Multiply                 | *        | integer * integer → integer | Left to Right   |
| 6           | Integer Divide           | div      | integer * integer → integer | Left to Right   |
| 6           | Modulus                  | mod      | integer * integer → integer | Left to Right   |
| 5           | Add                      | +        | integer * integer → integer | Left to Right   |
| 5           | Subtract                 | -        | integer * integer → integer | Left to Right   |
| 4           | Less Than                | <        | integer * integer → boolean | Not Associative |
| 4           | Greater Than             | >        | integer * integer → boolean | Not Associative |
| 4           | Less Than or Equal To    | <=       | integer * integer → boolean | Not Associative |
| 4           | Greater Than or Equal To | >=       | integer * integer → boolean | Not Associative |
| 3           | Equal To                 | ==       | any * any → boolean         | Left to Right   |
| 3           | Not Equal To             | !=       | any * any → boolean         | Left to Right   |
| 2           | Logical AND              | and      | boolean * boolean → boolean | Left to Right   |
| 1 (Lowest)  | Logical OR               | or       | boolean * boolean → boolean | Left to Right   |
