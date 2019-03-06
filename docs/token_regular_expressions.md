| Token |Regular Expression |
|---|---|
| posDigit |`1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9` |
| digit |`0 + posDigit` |
| integer |`0 + posDigit(digit)*` |
| letter | `[a-zA-Z]` |
| identifier | `letter(letter + digit)*` |
| Reserved Symbols | `";", "PRINT", "IF", "THEN", "ELSE", "FOR", "WHILE", "(", ")", "int", "bool", "true", "false", "{", "}", ",", "++", "++", "--", "--", "-", "not", "=", "or", "and", "==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "div", "mod", "^", "", ","` |
