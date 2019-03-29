# Intermediate Representation
## Difference from the book
We added a new constructor `RELOP` to the `Tree.exp` type and removed the `Cx` constructor of the `Translate.exp` type. In place of `Cx`, we use `Ex (Tree.RELOP ...)` instead. This change can hopefully help us derive simpler IR tree when results of comparisons are used as values directly.

## Assumptions
1. `initArray` allocates a chunk of memory of length `size + 1`, puts `size` at the first slot and return a pointer to the second slot.
2. In addition to the runtime-system functions mentioned in the book, there are also runtime-system functions `stringLessThan (s1, s2)`, `nilPointer ()` and `indexOutOfBound (index, bound)`.
