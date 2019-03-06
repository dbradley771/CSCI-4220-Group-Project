| Token |Regular Expression |
|---|---|
| _posDigit_ |`1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9` |
| _digit_ |`0 + posDigit` |
| _integer_ |`0 + posDigit(digit)*` |
| _letter_ | `[a-zA-Z]` |
| _identifier_ | `letter(letter + digit)*` |
| _Reserved Symbols_ | `";", "PRINT", "IF", "THEN", "ELSE", "FOR", "WHILE", "(", ")", "int", "bool", "true", "false", "{", "}", ",", "++", "++", "--", "--", "-", "not", "=", "or", "and", "==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "div", "mod", "^", "", ","` |
