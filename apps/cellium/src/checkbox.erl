-module(checkbox).
-moduledoc """
Checkbox widget module for togglable boolean options.

This module provides an interactive checkbox widget that displays a label
with a checkable box. When focused, colors are inverted.

## Usage

Basic checkbox:
```
checkbox:new(my_checkbox, "Enable feature")
```

Checkbox with initial checked state:
```
Widget = checkbox:new(my_checkbox, "Enable feature"),
CheckedWidget = Widget#{checked => true}
```

## Properties

- `label` (string): Text label displayed next to the checkbox
- `checked` (boolean): Whether the checkbox is checked. Default: false
- `focusable` (boolean): Set to true by default

## Display

- Unchecked: `[ ] Label`
- Checked: `[X] Label`
- When focused, foreground and background colors are swapped

## Event Handling

Toggle the checkbox state in your update function when handling
space or enter key events on this widget.
""".

-export([render/2, render_focused/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new checkbox with default label 'Checkbox'.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, "Checkbox").

-doc "Creates a new checkbox with the specified label.".
-spec new(term(), string()) -> map().
new(Id, Label) ->
    (widget:new())#{id => Id,
                    widget_type => checkbox,
                    label => Label,
                    checked => false,
                    focusable => true,
                    type => widget}.

-doc "Renders the checkbox in unfocused state.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Label = maps:get(label, Widget, ""),
    Checked = maps:get(checked, Widget, false),
    Mark = case Checked of true -> "X"; false -> " " end,
    cellium_buffer:put_string(X, Y, Fg, Bg, "[" ++ Mark ++ "] " ++ Label, Buffer).

-doc "Renders the checkbox in focused state with inverted colors.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Label = maps:get(label, Widget, ""),
    Checked = maps:get(checked, Widget, false),
    Mark = case Checked of true -> "X"; false -> " " end,
    cellium_buffer:put_string(X, Y, Bg, Fg, "[" ++ Mark ++ "] " ++ Label, Buffer).
