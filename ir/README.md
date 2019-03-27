# Intermediate Representation
## Differences from the book
We added two new constructors `RELOP` and `LVALUE` to the `Tree.exp` type and extracted two of its constructors `MEM` and `TEMP` into a new `Tree.lvalue` type. This change helps us put better constraints on our IR tree using SML's type system.

We also removed the `Cx` constructor of the `Translate.exp` type and use `Ex (Tree.RELOP ...)` in its place. This change can hopefully help us derive simpler IR tree when results of comparisons are used as values directly.

## Assumptions
1. `initArray` allocates a chunk of memory of length `size + 1`, puts `size` at the first slot and return a pointer to the second slot.
2. In addition to the runtime-system functions mentioned in the book, there are also runtime-system functions `stringLessThan (s1, s2)`, `nilPointer ()` and `indexOutOfBound (index, bound)`.
