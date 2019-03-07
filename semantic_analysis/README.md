# Semantic Analysis
## Special Features
### Fully Functional Types
We removed the `NAME` type and made our types fully functional by using a `unit->ty` function for the type of array elements and record fields.

### Error Handling
We introduced a `BOTTOM` type to be the type of the `break` expression as well as declarations and expressions whose type cannot be inferred due to errors. This type helps us prevent the cascading effect of type errors.