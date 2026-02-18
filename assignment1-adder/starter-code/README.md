# My Notes (Assignment 1: Adder)

## My Approach
I focused on building the compiler in clear stages: parse first, then code generation, then end-to-end testing with the Makefile pipeline.

I represented programs with an AST (`Expr`) so each operation (`add1`, `sub1`, `negate`, and numbers) is handled explicitly.  
In code generation, every expression compiles to instructions that leave the result in `rax`, which kept the logic simple and predictable.

## Design Choices
- I used pattern matching in the parser to keep invalid syntax handling strict.
- I generated straightforward assembly (`mov`, `add`, `sub`, `imul`) for readability.
- I kept the runtime minimal: call compiled code and print the returned value.

## Testing Strategy
I used both simple and nested test programs to check:
- literals and unary arithmetic
- mixed nesting patterns
- negative values and boundary-style values

I verified behavior by running `make test` and checking each `.run` output.
