-module(status_bar).
-moduledoc """
Status bar widget module for displaying application status.

This module provides a status bar widget typically used at the bottom of the
screen to display status information, help text, or current mode indicators.

## Usage

Basic status bar:
```
status_bar:new(main_status, "Ready")
```

Dynamic status bar with formatted text:
```
StatusText = io_lib:format("Line: ~p | Col: ~p", [Line, Col]),
{status_bar, [{id, status}], lists:flatten(StatusText)}
```

## Properties

- `text` (string): The status text to display. Default: empty string
- `width` (integer): Width of the status bar. Set by layout system
- `focusable` (boolean): Set to false by default (status bars are not focusable)

## Display

The status bar renders with inverted colors (background becomes foreground
and vice versa) and fills the entire width with the background color. Text
is displayed with one space padding on the left.

Format: ` Status text goes here                    `

## Common Use Cases

- Application status messages
- Keyboard shortcut hints
- Current mode or context information
- File information or cursor position
""".

-export([render/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new status bar with empty text.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, "").

-doc "Creates a new status bar with the specified text.".
-spec new(term(), string()) -> map().
new(Id, Text) ->
    (widget:new())#{id => Id,
                    widget_type => status_bar,
                    text => Text,
                    focusable => false,
                    type => widget}.

-doc """
Renders the status bar with inverted colors.

The entire width is filled with the background color, and text is rendered
with one space of left padding.
""".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Width = maps:get(width, Widget, 80),
    Text = maps:get(text, Widget, ""),

    %% Create a string of spaces for the background of the bar
    Background = lists:duplicate(Width, " "),
    Buffer1 = cellium_buffer:put_string(X, Y, Bg, Fg, Background, Buffer),

    %% Print the text on top of the inverted background
    cellium_buffer:put_string(X + 1, Y, Bg, Fg, Text, Buffer1).
