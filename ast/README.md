# Parser and Abstract Syntax Tree
## Shift-Reduce Conflicts
Without the precedence directory, our grammar have shift-reduce conflicts between:
1. Two binary operators (`+`, `-`, `*`, `/`, `=`, `<>`, `<`, `<=`, `>`, `>=`, `&`, `|`)
2. A binary operator and a `:=`, `do`, `then`, `else`, `of` or unary `-` operator.

These conflicts can all be resolved soundly by the precedence directory because there are clear association or precedence relations between these operators.

## Correction to Lexer
If we encounter an unexpected character in a formatting sequence, we assume that the programmer forgets to put the closing `\`. We report an `unclosed formatting sequence` error in this case. And if the unexpected character is not a `"`, we append the character to the current string and continue in the `STRING` state; if the character is a `"`, we end the string, generate the string token and go back to the `INITIAL` state.
