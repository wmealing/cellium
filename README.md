# Cellium

A declarative terminal UI framework for Erlang for building rich text-based terminal applications similar to how you write HTML.

Cellium implements the Elm Architecture pattern as a way to structure application logic, adapted for Erlang's concurrent execution model. This fits naturally with OTP patterns and makes terminal applications composable and testable.

**Table of Contents**

* [Cellium](#cellium)
  * [Getting Started](#getting-started)
    * [Building an Application](#building-an-application)
      * [init/1](#init1)
      * [update/2](#update2)
      * [render/1](#render1)
      * [Running It](#running-it)
  * [Views](#views)
    * [Containers and Boxes](#containers-and-boxes)
    * [Widgets](#widgets)
    * [Building Layouts](#building-layouts)
    * [Styling](#styling)
  * [Example Applications](#example-applications)
  * [Installation](#installation)

## Getting Started

Cellium applications follow a simple three-callback pattern: `init/1` for initial state, `update/2` for handling messages, and `render/1` for displaying the model.

### Building an Application

Let's build a simple counter application that displays an integer which can be incremented with `+` and decremented with `-`.

First, here's the complete example:

```erlang
-module(counter).

-behavior(cellium).

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{count => 0},
    {ok, Model}.

update(#{count := Count} = Model, Msg) ->
    case Msg of
        {tb_event, key, _, {keydata, _, $+}} ->
            #{count => Count + 1};
        {tb_event, key, _, {keydata, _, $-}} ->
            #{count => Count - 1};
        {tb_event, key, _ , {keydata, _ , $q}} ->
            init:stop();
        _Other ->
            Model
    end.

render(#{count := Count}) ->
    Text = io_lib:format("Counter: ~p (+/-)", [Count]),
    #{
        type => container,
        orientation => horizontal,
        children => [
            #{
                type => widget,
                widget_type => text,
                value => Text
            }
        ]
    }.

start() ->
    cellium:start(?MODULE).
```

The application declares itself as implementing the `cellium` behavior, which ensures it provides the required callbacks.

#### `init/1`

The `init/1` callback defines the initial model. The model is the Elm architecture's term for application state. It can be any Erlang term, though maps and records are convenient for organizing larger state structures.

```erlang
init(_Args) ->
    Model = #{count => 0},
    {ok, Model}.
```

The function returns `{ok, Model}` to indicate successful initialization.

#### `update/2`

The `update/2` callback transforms the model when a message is received. The framework automatically calls `update/2` when terminal events occur: key presses, mouse events, window resizing, etc.

```erlang
update(#{count := Count} = Model, Msg) ->
    case Msg of
        {tb_event, key, _ ,{keydata, _ ,$+}} ->
            #{count => Count + 1};
        {tb_event, key, _ ,{keydata, _ ,$-}} ->
            #{count => Count - 1};
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            init:stop();
        _AnythingElse ->
            Model
    end.
```

It's good practice to include a catch-all clause to handle unexpected messages gracefully rather than crashing the application.

#### `render/1`

The `render/1` callback returns a view tree describing what to display. Views are maps that describe the structure of widgets, their properties, and their layout.

```erlang
render(#{count := Count}) ->
    Text = io_lib:format("Counter: ~p (+/-)", [Count]),
    #{
        type => container,
        orientation => horizontal,
        children => [
            #{
                type => widget,
                widget_type => text,
                value => Text
            }
        ]
    }.
```

The root of the view tree should always be a container. Child elements are organized in the `children` list.

#### Running It

Start the application by calling:

```sh
$ erl -noshell -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'counter:start()'
```

Using the -noinput parameter so that the erlang runtime doesn't 'steal' keyboard input that is intended for your application.

```erlang
counter:start().
```

Or, compile and run from the shell:

```bash
$ rebar3 compile
$ rebar3 shell
```

Then from the Erlang shell:

```erlang
1> counter:start().
```

You'll see the counter, be able to change it with `+` and `-`, and quit with `q`.

## The Model

Views in Cellium are trees of components describing your terminal interface. They're built from containers that organize layout and widgets that display content.

### Containers and Boxes

A container is the fundamental layout unit. The `orientation` field controls whether children are laid out horizontally or vertically. Use `horizontal` for side-by-side layouts and `vertical` for stacked layouts.

```erlang
render(Model) ->
    #{
        type => container,
        orientation => vertical,
        children => [
            top_bar(),
            content_area(),
            bottom_bar()
        ]
    }.

top_bar() ->
    #{
        type => widget,
        widget_type => text,
        value => "Application Title"
    }.

content_area() ->
    #{
        type => container,
        orientation => horizontal,
        children => [
            left_panel(),
            right_panel()
        ]
    }.

left_panel() ->
    #{
        type => widget,
        widget_type => text,
        value => "Left content"
    }.

right_panel() ->
    #{
        type => widget,
        widget_type => text,
        value => "Right content"
    }.
```

### Widgets

Widgets are leaf nodes in the view tree that display actual content. Common widget types include `text`, `text_input`, `button`, and `table`.

Each widget accepts a `value` or relevant property containing its content, and optional styling properties like `color` and `background`.

```erlang
#{
    type => widget,
    widget_type => text,
    value => "Hello, world!",
    color => red
}
```

### Building Layouts

Break complex layouts into smaller functions. This keeps renders readable and makes layout logic testable and reusable.

```erlang
render(Model) ->
    #{
        type => container,
        orientation => vertical,
        children => [
            header(Model),
            main_content(Model),
            footer(Model)
        ]
    }.

header(Model) ->
    #{
        type => widget,
        id => header_text,
        class => header,
        value => <<"HEADER WORDING">>
    }.

main_content(Model) ->
    case Model of
        #{tab => details} -> render_details(Model);
        #{tab => settings} -> render_settings(Model);
        _ -> render_default(Model)
    end.

render_details(Model) ->
    #{
        type => widget,
        widget_type => text,
        value => format_details(Model)
    }.
```

### Styling

Attributes control the appearance of widgets:

```erlang
#{
    type => widget,
    widget_type => text,
    value => "Red text",
    color => red
}

#{
    type => widget,
    widget_type => text,
    value => "Black on white",
    color => black,
    background => white
}
```

## Example Applications

The `examples` directory contains sample applications demonstrating different features:

- `counter.erl`: Simple state and event handling
- `text_input.erl`: Capturing and processing user input
- `multi_view.erl`: Switching between different rendered views
- `table.erl`: Displaying tabular data
- `layout_demo.erl`: Complex multi-column layouts

Start an example with:

```bash
$ rebar3 compile
$ rebar3 shell
```

Then from the shell:

```erlang
1> example_name:start().
```

Press `q` to quit any example.

## Installation

Add Cellium to your `rebar.config`:

```erlang
{deps, [
    {cellium, {git, "https://github.com/yourusername/cellium.git", {branch, "main"}}}
]}.
```

Then fetch and compile:

```bash
$ rebar3 get-deps
$ rebar3 compile
```

## Build

```bash
$ rebar3 compile
```

## License

This software is released under the MIT License.
