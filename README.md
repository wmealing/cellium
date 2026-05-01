
![Cellium Project Logo](priv/logo.png)

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
-include("cellium.hrl").
-export([init/1, update/2, render/1, start/0]).

init(_Args) ->
    InitialCount = 0,
    Model = #{
        count => InitialCount,
        widget_states => #{
            display => #{text => io_lib:format("Count: ~p", [InitialCount])}
        }
    },
    {ok, Model}.

update(Model = #{count := Count, widget_states := States}, Msg) ->
    case Msg of
        {button_clicked, plus_btn} ->
            NewCount = Count + 1,
            Model#{
                count => NewCount,
                widget_states => States#{display => #{text => io_lib:format("Count: ~p", [NewCount])}}
            };
        {button_clicked, minus_btn} ->
            NewCount = Count - 1,
            Model#{
                count => NewCount,
                widget_states => States#{display => #{text => io_lib:format("Count: ~p", [NewCount])}}
            };
        {key, _, _, _, _, <<"q">>} -> cellium:stop(), Model;
        _ -> Model
    end.

render(_Model) ->
    {vbox, [{padding, 1}], [
        {header, [], "Counter Example"},
        {text, [{id, display}]},
        {hbox, [{size, 1}], [
            {button, [{id, minus_btn}, {size, 5}], "-"},
            {spacer, [{size, 2}]},
            {button, [{id, plus_btn}, {size, 5}], "+"}
        ]},
        {text, [], "Press Tab to focus, Space/Enter to click, 'q' to quit"}
    ]}.

start() ->
    cellium:start(#{module => ?MODULE}).
```

## Automatic Component State

Cellium simplifies interactive UIs by automatically managing the state of complex widgets. This "Component Pattern" is inspired by modern frameworks like **React** (Controlled Components) and **Phoenix LiveView** (LiveComponents).

Instead of manually threading every keypress and update:
1. **Event Routing**: When a widget has focus, the framework automatically routes keyboard events to that widget's internal handler.
2. **State Storage**: Internal state is stored in a `widget_states` map within your Model, keyed by the widget's `id`.
3. **Automatic Injection**: During rendering, the DSL lookups the stored state by ID and merges it into the widget properties.

## Widget Reference

### Core Widgets
*   `text`: Simple text display.
*   `button`: Interactive clickable button. Emits `{button_clicked, Id}`.
*   `text_input`: Multi-line or single-line text editor with cursor management.
*   `checkbox`: Boolean toggle with label.
*   `list`: Vertically scrollable list of selectable items.

### Data Visualization
*   **`tree`**: Hierarchical navigation with support for expansion/collapsing.
    ```erlang
    {tree, [{id, my_tree}], [{"Root", [{"Child 1", []}, {"Child 2", []}]}]}
    ```
*   **`radiogroup`**: Mutually exclusive selection group. Supports `horizontal` and `vertical` layouts.
    ```erlang
    {radiogroup, [{id, rg1}, {orientation, horizontal}], [opt1, opt2, opt3]}
    ```
*   **`progress_bar`**: Visual task completion tracker using block characters (`[████░░░░]`).
    ```erlang
    {progress_bar, [{id, pb1}, {progress, 0.75}, {width, 20}]}
    ```
*   **`gauge`**: Labeled percentage indicator, useful for levels or volume.
    ```erlang
    {gauge, [{id, g1}, {label, <<"Vol">>}, {value, 50}, {width, 30}]}
    ```

### Containers
*   `box`: Simple rectangular container with a border.
*   `frame`: Container with a border and an optional title.
*   `vbox` / `hbox`: Layout containers for vertical and horizontal stacking.
*   `tabs`: Multi-tab interface for switching between views.

## Screen Management

Cellium provides a screen management system for applications with multiple views. The `screen` module handles lifecycle, transitions, and automatic focus cleanup.

### Basic Screen Transitions

```erlang
SearchScreen = screen:new(search_screen,
    cellium_dsl:from_dsl({vbox, [], [
        {text_input, [{id, search_box}, {focusable, true}]},
        {list, [{id, results_list}, {focusable, true}]}
    ]})),

% Transition between screens (automatic cleanup)
NewScreen = screen:transition(OldScreen, SearchScreen)
```

## Layout System

Cellium uses a flexible layout engine that calculates absolute coordinates based on container constraints.

- **Space Distribution**: Use `{size, N}` for fixed height/width, or `{expand, true}` to fill remaining space.
- **Padding**: Apply `{padding, 1}` to any container to create inner margins. Containers like `box`, `frame`, and `dialog` default to 1-character padding.
- **Borders**: Automatically switch between single-line (`┌─┐`) and double-line (`╔═╗`) based on focus state.

## UI Pipeline

1.  **DSL**: `render/1` returns a high-level tuple tree.
2.  **Processing**: `cellium_dsl` converts tuples into internal widget maps and injects state.
3.  **Layout**: `layout` engine calculates absolute (x, y) coordinates and dimensions.
4.  **Styling**: `css` engine applies visual themes.
5.  **Rendering**: `view` process uses the `native_terminal` driver to draw to the screen.

## Development & Testing

### Snapshot Testing
Cellium includes a powerful "Snapshot" testing pattern that allows you to verify widget rendering without a real terminal.

1.  **Declaration**: Define the widget in DSL.
2.  **Rendering**: Pipe the widget into a virtual buffer.
3.  **Assertion**: Compare the buffer against an "Idealized" ASCII string.

Use the `cellium_test_utils:assert_snapshot/2` and `show_render/2` helpers to see the output during your tests.

### Build and Run

```bash
rebar3 compile
make run example=widgets_gallery
```

## Project Structure

- `src/`: Core framework source code.
- `test/`: Snapshot tests and unit tests.
- `examples/`: Sample applications.
- `include/`: Common header files.

### Documentation

- **API docs**: [https://wmealing.github.io/cellium/api-reference.html](https://wmealing.github.io/cellium/api-reference.html)
- **Discussion**: [https://wmealing.github.io/](https://wmealing.github.io/)
