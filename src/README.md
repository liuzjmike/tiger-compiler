# Tiger Compiler

## Fully Functional Types
We remove the `NAME` type in `Types.ty` and use `unit -> ty` functions in its place.

## Type Errors
We introduce a `BOTTOM` type to be the type of the `break` expression. It also serves as a placeholder when types cannot be inferred due to errors. This helps us prevent the cascading effect of type errors.
