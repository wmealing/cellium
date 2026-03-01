## The Developer's Pact: Our Rules of Engagement

This document outlines the core principles and conventions we will follow in this project. As my AI partner, your adherence to these rules is critical for building high-quality, maintainable software.


🏛️ Principle 1: Architecture & Structure

Modularity is Key: No single file should exceed 500 lines. If it grows too large, your first step is to propose a refactoring plan to break it into smaller, logical modules.

Consistent Organization: We group files by feature. 

Environment First: All sensitive keys, API endpoints, or configuration variables must be managed through a .env file and loaded using dotenv_config_erlang. 

Every project should be created using 'rebar3' style structure (e.g. using 'rebar3 new app').

✅ Principle 2: Quality & Reliability

Test Everything That Matters: Every new function, class, or API endpoint must be accompanied by unit tests in the tests/ directory.  Only create eunit tests.

The Test Triad: For each feature, provide at least three tests:

- A "happy path" test for expected behavior.

- An "edge case" test for unusual but valid inputs.

- A "failure case" test for expected errors or invalid inputs.

Every function must have a docstring for the module, functions and types:

Module doc:

```erlang
-module(my_module).
-moduledoc """
A comprehensive module for handling various data operations.
It provides functions for list manipulation, string processing,
and basic arithmetic.
""".

```

Function doc:

```eerlang
-doc "Adds two numbers and returns their sum.".
add(A, B) -> A + B.
```

Type docs:

```erlang
-type my_record() :: #{field1 => integer(), field2 => string()}.
-doc "Represents a custom record for storing structured data.".
-type my_record() :: #{field1 => integer(), field2 => string()}.
```
✍️ Principle 3: Code & Style

Follow the Standards: All Erlang code must be formatted with `rebar3 fmt` and adhere to the [Erlang/OTP Style Guide](https://www.erlang.org/doc/programming_examples/programming_rules.html).

Data Certainty: Leverage Erlang's type specifications and use Dialyzer (run via `rebar3 dialyzer`) to enforce data shapes and catch inconsistencies before runtime. This is our single source of truth for data shapes.

Always add dependancies to rebar.config first, and check they compile with rebar, before using them.

🧠 Principle 4: Your Behavior as an AI

Clarify, Don't Assume: If a requirement is ambiguous or context is missing, your first action is to ask for clarification.

No Hallucinations: Do not invent libraries, functions, or file paths. If you need a tool you don't have, state what you need and why.

Plan Before You Code: For any non-trivial task, first outline your implementation plan in a list or with pseudocode. We will approve it before you write the final code.

Explain the "Why": For complex or non-obvious blocks of code, add a %%  WHY: comment explaining the reasoning behind the implementation choice.

📚 Principal 5: Good libraries to use.

Erlang programming rules: https://www.erlang.org/doc/programming_examples/programming_rules.html
Using dotenv_config: https://github.com/dziaineka/dotenv_config_erlang

💳Project layout.

├── apps
│   └── cellium
│       └── src
│           ├── cellium_app.erl
│           ├── cellium_renderer_server.erl <-- this does the actual rendering
│           ├── cellium_sup.erl <-- keeps the renderer running.
│           ├── cellium.app.src
│           ├── input_handler.erl
│           ├── layout_engine.erl 
│           └── widgets.erl <-- functions common for widgets.
├── config
│   ├── sys.config
│   └── vm.args
├── GEMINI.md
├── include
│   ├── cellium.hrl <-- common macros/defines
│   └── bbox.hrl <-- bounding box record.
├── Makefile
├── .env <-- standard .env file.
├── README.md
└── rebar.config <-- the gloabl rebar.config


