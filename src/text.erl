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
    Width = maps:get(width, Widget, ?TERMINAL:term_width()),

    % Split by newline first to preserve user-defined lines
    TextLines = string:split(Text, "\n", all),
    
    {_, FinalBuffer} = lists:foldl(fun(Line, {CurrentY, AccBuffer}) ->
        case Wrap of
            true ->
                % Wrap this specific line at widget width
                LineBin = list_to_binary(Line),
                WrappedLines = case Width > 0 of
                    true -> greedy_wrap:word_wrap(LineBin, Width);
                    false -> [LineBin]
                end,
                NewBuffer = render_lines(X, CurrentY, Fg, Bg, WrappedLines, AccBuffer),
                {CurrentY + length(WrappedLines), NewBuffer};
            false ->
                % No wrapping - render as single line
                NewBuffer = cellium_buffer:put_string(X, CurrentY, Fg, Bg, Line, AccBuffer),
                {CurrentY + 1, NewBuffer}
        end
    end, {Y, Buffer}, TextLines),
    FinalBuffer.

render_lines(_X, _Y, _Fg, _Bg, [], Buffer) ->
    Buffer;
render_lines(X, Y, Fg, Bg, [Line | Rest], Buffer) ->
    LineStr = binary_to_list(Line),
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, LineStr, Buffer),
    render_lines(X, Y + 1, Fg, Bg, Rest, NewBuffer).
