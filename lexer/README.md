# Lexer
## Comment Handling
Comments are handled using a start state called `COMMENT`. In addition, we keep track of the depth of nexted comments using the `commentDepth` variable. When we see the comment start symbol `/*` in the `INITIAL` state, we enter the `COMMENT` state and increment `commentDepth` (also incremented when when we see `/*` in the `COMMENT` state). When we see the comment end symbol `*/` in the `COMMENT` state we decrement `commentDepth` and if `commentDepth` goes back to 0 we return to the `INITIAL` state.

## String Handling
Strings are handled using two start states `STRING` and `ESCAPE`. When we see `"` in the `INITIAL` state we enter the `STRING` state and when we see `\` in the `STRING` state we enter the `ESCAPE` state. When we are in either of these states, the string currently being built is stored in the `currentString` variable. The `STRING` state is responsible for appending non-escape characters to `currentString` and the `ESCAPE` state is responsible for interpreting escape characters and then append them to `currentString`.

## Error Handling
Our lexer detects 7 types of errors:
 1. Unclosed comment
 2. Unclosed string
 3. Illegal identifier name
 4. Illegal character in the program
 5. Illegal escape character in strings
 6. Illegal control character in strings
 7. Illegal character in the formatting sequences in strings

In particular, we detect unclosed strings in two ways: 1) if we see a newline character `\n` in the middle of a string, or 2) in case the programmer does not correctly end the program file with a newline and has an unclosed string at the end, we keep track of whether we are in the middle of a string using the `inString` variable and report an error if it is no `false` at the end of the file.

## End-of-File Handling
As previously mentioned, we have a `commentDepth` variable which keeps track of the depth of nested comments in the program and a `inString` character which keeps track of whether we are in the middle of a string. If `commentDepth` is greater than 0 at the end of the file, we report an `unclosed comment` error. If `inString` is `true` at the end of the file, we report an `unclosed string` error. We reset these two variables as well as the `ErrorMsg` structure at the end of the file.