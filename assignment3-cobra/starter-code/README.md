# My Notes (Assignment 3: Cobra)

## My Approach
I introduced tagged values first (numbers and booleans), then layered in conditionals, blocks, loops, mutation, and runtime checks.

I kept the compiler pipeline expression-oriented: each compiled expression leaves a tagged value in `rax`.

## Design Choices
- I used tag-bit checks before numeric operations.
- I generated unique labels for `if`, `loop`, and `break` control flow.
- I implemented runtime helpers for input parsing, value printing, and error reporting.

## Testing Strategy
I added tests for:
- boolean literals and comparisons
- `if`, `block`, `loop`, and `break`
- `set!` mutation patterns
- `isnum` and `isbool`
- programs using `input`

I ran `make test` and validated both numeric and boolean outputs.
