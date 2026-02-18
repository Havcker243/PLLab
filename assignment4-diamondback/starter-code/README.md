# My Notes (Assignment 4: Diamondback)

## My Approach
I extended Cobra by adding program-level function definitions and calls, including recursive calls and arity validation.

I implemented function compilation with explicit call labels and stack-frame based variable access.

## Design Choices
- I parse full programs as `(fun ...)*` plus one main expression.
- I maintain a function-arity table to catch invalid calls at compile time.
- I support built-in `print` via runtime interop while keeping tagged value behavior consistent.

## Testing Strategy
I added tests for:
- single and multi-argument functions
- call chains and nested calls
- recursion and mutual recursion
- function-local `let` and `set!`
- boolean/numeric behavior inside functions

I used `make test` and spot-checked generated assembly for call/return behavior.
