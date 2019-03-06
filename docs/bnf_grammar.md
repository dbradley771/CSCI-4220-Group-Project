# BNF Grammar

| LHS |  | RHS |
|---|---|---|
| \<program\> | ::= | \<statement-list\> |
| \<statement-list\> | ::= | \<statement\> ";" \<statement-list\> |
|  | \| | \<statement\> ";" |
| \<statement\> | ::= | \<declaration\> |
|  | \| | \<assignment\> |
|  | \| | \<print\> |
|  | \| | \<conditional\> |
|  | \| | \<loop\> |
| \<declaration\> | ::= | \<typed-variable\> |
| \<assignment\> | ::= | \<pre-post\> |
|  | \| | \<typed-variable\> "=" \<expression\> |
|  | \| | _identifier_ "=" \<expression\> |
| \<typed-variable\> | ::= | "int" _identifier_ |
|  | \| | "bool" _identifier_ |
| \<print\> | ::= | "PRINT" "(" \<expression\> ")" |
| \<conditional\> | ::= | \<if\> |
|  | \| | \<if-else\> |
| \<if\> | ::= | "IF" "(" \<expression\> ")" "THEN" \<block\> |
| \<if-else\> | ::= | "IF" "(" \<expression\> ")" "THEN" \<block\> "ELSE" \<block\> |
| \<loop\> | ::= | \<for-loop\> |
|  | \| | \<while-loop\> |
| \<for-loop\> | ::= | "FOR" "(" \<assignment\> ";" \<expression\> ";" \<assignment\> ")" \<block\> |
| \<while-loop\> | ::= | "WHILE" "(" \<expression\> ")" \<block\> |
| \<block\> | ::= | "{" \<statement-list\> "}" |
| \<expression\> | ::= | \<logic-or\> |
| \<logic-or\> | ::= | \<logic-or\> "OR" \<logic-and\> |
|  | \| | \<logic-and\> |
| \<logic-and\> | ::= | \<logic-and\> "AND" \<equality\> |
|  | \| | \<equality\> |
| \<equality\> | ::= | \<equality\> "==" \<inequality\> |
|  | \| | \<equality\> "!=" \<inequality\> |
|  | \| | \<inequality\> |
| \<inequality\> | ::= | \<additive\> "\<" \<additive\> |
|  | \| | \<additive\> "\<=" \<additive\> |
|  | \| | \<additive\> "\>" \<additive\> |
|  | \| | \<additive\> "\>=" \<additive\> |
|  | \| | \<additive\> |
| \<additive\> | ::= | \<additive\> "+" \<multiplicative\> |
|  | \| | \<additive\> "-" \<multiplicative\> |
|  | \| | \<multiplicative\> |
| \<multiplicative\> | ::= | \<multiplicative\> "*" \<unary-minus\> |
|  | \| | \<multiplicative\> "div" \<unary-minus\> |
|  | \| | \<multiplicative\> "mod" \<unary-minus\> |
|  | \| | \<unary-minus\> |
| \<unary-minus\> | ::= | "-" \<unary-minus\> |
|  | \| | \<exponent\> |
| \<exponent\> | ::= | \<operation\> "^" \<exponent\> |
|  | \| | \<operation\> |
| \<operation\> | ::= | "(" \<expression\> ")" |
|  | \| | "\|" \<expression\> "\|" |
|  | \| | "NOT" "(" \<expression\> ")" |
|  | \| | \<pre-post\> |
|  | \| | _identifier_ |
|  | \| | \<value\> |
| \<pre-post\> | ::= | "++" _identifier_ |
|  | \| | "--" _identifier_ |
|  | \| | _identifier_ "++" |
|  | \| | _identifier_ "--" |
| \<value\> | ::= | _integer_ |
|  | \| | "TRUE" |
|  | \| | "FALSE" |
