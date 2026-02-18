# My Notes (Assignment 2: Boa)

## My Approach
I extended the compiler from Adder by first adding variable support and `let` bindings, then binary operators, and finally edge-case checks.

I used an environment map to track stack slots for variables and compiled expressions with a running stack offset. This made nested scopes and shadowing manageable.

## Design Choices
- I modeled unary and binary operators explicitly in the AST.
- I evaluated binary expressions left-to-right and stored temporary values on the stack.
- I used strict parser patterns so malformed `let` bindings fail early.

## Testing Strategy
I added tests for:
- simple and nested `let`
- shadowing behavior
- all arithmetic operators
- mixed expressions with unary and binary ops

I verified correctness with `make test` and checked representative generated assembly files.
