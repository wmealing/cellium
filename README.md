
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

## Screen Management

Cellium provides a screen management system for applications with multiple views (e.g., search screen, customer form, settings dialog). The `screen` module handles screen lifecycle, transitions, and automatic focus cleanup.

### Screen Lifecycle

Screens have four states:
- **Created**: Widget tree built but not registered
- **Shown**: Active, widgets registered with focus manager
- **Hidden**: Inactive, widgets unregistered but preserved
- **Destroyed**: Permanently removed

### Basic Screen Transitions

```erlang
% Create screens
SearchScreen = screen:new(search_screen,
    cellium_dsl:from_dsl({vbox, [], [
        {text_input, [{id, search_box}, {focusable, true}]},
        {list, [{id, results_list}, {focusable, true}]}
    ]})),

CustomerScreen = screen:new(customer_form,
    cellium_dsl:from_dsl({vbox, [], [
        {text_input, [{id, name_field}, {focusable, true}]},
        {button, [{id, save_btn}, {focusable, true}], "Save"}
    ]})),

% Transition between screens (automatic cleanup)
NewScreen = screen:transition(SearchScreen, CustomerScreen)
```

### Screen Stack

For modal dialogs or nested navigation, use the screen stack:

```erlang
% Push a dialog (current screen hidden but preserved)
screen:push(ConfirmDialog),
% Pop back to previous screen
screen:pop(),

% Replace current screen entirely
screen:replace(NewScreen)
```

### Dynamic Screens with Builders

For screens that need fresh data on each display:

```erlang
screen:new(
    customer_form,
    fun() ->
        Data = fetch_customer_data(),
        build_customer_form(Data)
    end,
    empty_widget()
)
```

The builder function is called each time the screen is shown, ensuring data is current.

## Layout System

Cellium uses a flexible layout engine that calculates absolute coordinates and dimensions based on container constraints and widget properties.

### Positioning Modes

- **Relative (Default)**: Widgets are positioned by their parent container (`vbox`, `hbox`, `grid`, etc.) according to the container's orientation and expansion rules.
- **Absolute**: By setting `{position, absolute}`, a widget bypasses the layout engine. You must manually provide `{x, X}`, `{y, Y}`, `{width, W}`, and `{height, H}`.

### Space Distribution

In relative layout, space is distributed along the container's primary axis (vertical for `vbox`, horizontal for `hbox`):

1.  **Fixed Size**: Use `{size, N}` to request a fixed number of characters in the primary axis.
2.  **Expansion**: Use `{expand, true}` to request that the widget fill the remaining available space.
3.  **Automatic Splitting**: If multiple widgets have `{expand, true}`, they split the remaining space equally.
4.  **Default Behavior**: If neither `size` nor `expand` is specified, a default `{size, 1}` is applied.

### Padding

Padding can be applied to any container or widget to create space between the border and the content:

- **Uniform**: `{padding, 1}` (1 character on all four sides).
- **Specific**: `{padding, #{top => 1, bottom => 1, left => 2, right => 2}}`.

### Width and Height

While `size` controls the dimension along the container's primary axis, `width` and `height` can be used to explicitly set the dimension along the cross-axis or for absolutely positioned widgets.

## UI Pipeline

1.  **DSL**: The `render/1` function returns a high-level DSL (e.g., `{vbox, Props, Children}`).
2.  **Processing**: `cellium_dsl` converts the DSL into a tree of internal widget maps.
3.  **Layout**: The `layout` engine calculates absolute coordinates (x, y) and final dimensions for every widget based on constraints and expansion rules.
4.  **Styling**: A CSS-like engine (`css`) applies visual properties (colors, borders) from cached stylesheets.
5.  **Rendering**: The `view` process utilizes the `native_terminal` driver to draw the final representation to the terminal screen.

## Project Structure

- `src/`: Core framework source code, including the layout engine and terminal drivers.
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
- **Logging**: Logs are written to `./logs/cellium-debug` by default. Logging configuration is managed in `src/logging.erl`.

### Documentation

- **API docs** [https://wmealing.github.io/cellium/api-reference.html](https://wmealing.github.io/cellium/api-reference.html "API docs")
- **Occasional discussions** [https://wmealing.github.io/](https://wmealing.github.io/ "Ocassional posts on cellium")
