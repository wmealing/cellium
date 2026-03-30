-module(header).
-moduledoc """
Header widget module for displaying section titles.

This module provides a simple header widget for displaying prominent text,
typically used for section titles, headings, or labels in the UI.

## Usage

Basic header:
```
header:new(page_title, "Settings")
```

Header with custom color:
```
{header, [{id, title}, {color, cyan}], "Application Settings"}
```

## Properties

- `text` (string or binary): The header text to display
- `color` (atom): Text color. Inherited from widget properties

## Display

The header renders as plain text at the specified position. It does not
change appearance when focused (render and render_focused behave the same).

## Common Use Cases

- Page or section titles
- Category labels
- Divider labels in forms or menus
""".

-export([render/2, render_focused/2, new/2]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new header widget with the specified text.".
-spec new(term(), string() | binary()) -> map().
new(Id, Text) ->
    (widget:new())#{id => Id, widget_type => header, type => widget, text => Text}.

-doc "Renders the header text.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = maps:get(text, Widget, <<>>),
    cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer).

-doc "Renders the header in focused state (identical to unfocused).".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    render(Widget, Buffer).
