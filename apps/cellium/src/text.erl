-module(text).
-moduledoc """
Text widget module for displaying static text.

This module provides a simple text widget that can display static text
with optional word wrapping.

## Usage

Basic text widget:
```
text:new(my_text, "Hello, World!")
```

Text widget with wrapping (requires expand or size to get width from layout):
```
Widget = text:new(my_text, "This is a long text that will wrap"),
WrappingWidget = Widget#{wrap => true, expand => true}
```

## Properties

- `wrap` (boolean): Enable word wrapping. When true, text wraps at the widget's
  width (set by the layout system). Default: false
- `expand` (boolean): Request the layout system to assign width and height.
  Required when using wrap. Default: false
- `width` (integer): Set by the layout system when expand is true
""".

-export([render/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new text widget with empty content.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, "").

-doc "Creates a new text widget with the specified content.".
-spec new(term(), string()) -> map().
new(Id, Content) ->
    (widget:new())#{id => Id,
                    widget_type => text,
                    text => Content,
                    type => widget}.

-doc """
Renders the text widget to the buffer.

If wrap is enabled and width is set by layout, text wraps at word boundaries
at the widget's width.
""".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = maps:get(text, Widget, ""),
    Wrap = maps:get(wrap, Widget, false),

    case Wrap of
        true ->
            % Wrap text at widget width (set by layout) and render each line
            Width = maps:get(width, Widget),
            TextBin = list_to_binary(Text),
            Lines = greedy_wrap:word_wrap(TextBin, Width),
            render_lines(X, Y, Fg, Bg, Lines, Buffer);
        false ->
            % No wrapping - render as single line
            cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer)
    end.

render_lines(_X, _Y, _Fg, _Bg, [], Buffer) ->
    Buffer;
render_lines(X, Y, Fg, Bg, [Line | Rest], Buffer) ->
    LineStr = binary_to_list(Line),
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, LineStr, Buffer),
    render_lines(X, Y + 1, Fg, Bg, Rest, NewBuffer).
