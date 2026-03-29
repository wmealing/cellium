# Cellium

Cellium is a Terminal User Interface (TUI) framework for Erlang/OTP. It provides a declarative way to build interactive terminal applications using an architecture inspired by the Elm Architecture.

## Core Architecture

Applications built with Cellium implement the `cellium` behaviour, which follows a strictly decoupled pattern:

1.  **Model**: The application state.
2.  **Update**: A function that transforms the model in response to messages (keyboard input, resize events, or internal timers).
3.  **View**: A function that transforms the model into a UI representation using a tuple-based DSL.

### The `cellium` Behaviour

To create an application, implement the following callbacks:

- `init(Args)`: Initializes the application model.
- `update(Model, Msg)`: Processes events and returns the updated model.
- `render(Model)`: Returns the UI structure as a DSL tree.

## Example

```erlang
-module(counter).
-behaviour(cellium).
-export([init/1, update/2, render/1, start/0]).

init(_Args) ->
    {ok, #{count => 0}}.

update(Model = #{count := Count}, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"+">>} -> Model#{count => Count + 1};
        {key, _, _, _, _, <<"-">>} -> Model#{count => Count - 1};
        {key, _, _, _, _, <<"q">>} -> cellium:stop(), Model;
        _ -> Model
    end.

render(#{count := Count}) ->
    {vbox, [{padding, 1}], [
        {header, [], "Counter Example"},
        {text, [], io_lib:format("Count: ~p", [Count])},
        {text, [], "Press + or - to change, 'q' to quit"}
    ]}.

start() ->
    cellium:start(#{module => ?MODULE}).
```

## UI Pipeline

1.  **DSL**: The `render/1` function returns a high-level DSL (e.g., `{vbox, Props, Children}`).
2.  **Processing**: `cellium_dsl` converts the DSL into a tree of internal widget maps.
3.  **Layout**: The `layout` engine calculates absolute coordinates (x, y) and final dimensions for every widget based on constraints and expansion rules.
4.  **Styling**: A CSS-like engine (`css`) applies visual properties (colors, borders) from cached stylesheets.
5.  **Rendering**: The `view` process utilizes the `native_terminal` driver to draw the final representation to the terminal screen.

## Project Structure

- `apps/cellium/src/`: Core framework source code, including the layout engine and terminal drivers.
- `examples/`: Sample applications demonstrating widgets and architectural patterns.
- `include/`: Common header files and macro definitions.
- `priv/`: Default stylesheets and theme configuration.

## Build and Run

### Prerequisites

- Erlang/OTP 26 or later.
- rebar3.

### Compilation

```bash
rebar3 compile
```

### Running Examples

Use the provided `Makefile` to execute example applications:

```bash
make run example=counter
make run example=widgets_gallery
```

## Development

- **Testing**: `rebar3 eunit`
- **Logging**: Logs are written to `./logs/debug` by default. Logging configuration is managed in `apps/cellium/src/logging.erl`.
