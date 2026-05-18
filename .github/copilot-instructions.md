# Copilot Instructions

## Project context

- This project is implemented in Zig.
- Target Zig version is `0.16.0`.
- Do not suggest APIs or syntax from older or newer Zig versions unless clearly marked as an alternative.
- The developer is new to Zig. Prefer clear, explicit, idiomatic code over clever abstractions.
- This project is `ft_lex`: a lexer generator that reads lex input and emits C output.

## How to help

- Be strict about Zig 0.16 correctness.
- If a suggested approach is shaky, non-idiomatic, or likely wrong, say so directly.
- Point out lifetime, allocator, error handling, union, enum, slice, and pointer mistakes early.
- Prefer small steps and simple data structures.
- Explain why a Zig pattern is good or bad when it is not obvious.
- When there are tradeoffs, recommend one approach instead of dumping many equivalent options.

## Coding style

- Prefer minimal code.
- Prefer `snake_case` names.
- Prefer explicit types when they improve readability for a beginner.
- Avoid unnecessary generics, metaprogramming, and premature abstractions.
- Keep functions focused and easy to test.
- Add comments only when they explain something non-obvious.

## Zig-specific guidance

- Prefer standard library APIs only.
- Be careful with allocator ownership and deinitialization responsibilities.
- Do not hide allocations.
- Use tagged unions when node kind matters.
- Avoid unsafe pointer patterns unless they are clearly justified.
- Do not invent helper APIs that do not exist in Zig 0.16.
- If code depends on build system behavior, keep `build.zig` examples compatible with Zig 0.16.

## Review behavior

- Check for "bullshit" actively: wrong assumptions, overengineered code, fake idioms, or suspicious use of Zig features.
- If code is technically valid but poor for this project, explain the issue and suggest a simpler alternative.
- Flag likely bugs before style nits.
- Prefer correctness and clarity over cleverness.

## Project constraints

- The final executable must be named `ft_lex`.
- Generated scanner output targets C.
- `libl` and generated C output must stay within project constraints and allowed C library usage.
- Design regex, parser, automata, and codegen pieces so they can evolve incrementally.
