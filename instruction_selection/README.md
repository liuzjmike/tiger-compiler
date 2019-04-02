# Instruction Selection
## Implementation Note
In order to support automated testing, our `Translate.getResult` funtion resets the internal `Translate.fragList`. Therefore, `Translate.getResult` should only be called once after the compilation of a tiger source file.
