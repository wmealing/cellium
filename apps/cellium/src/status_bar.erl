-module(status_bar).
-export([render/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, "").

-spec new(term(), string()) -> map().
new(Id, Text) ->
    (widget:new())#{id => Id,
                    widget_type => status_bar,
                    text => Text,
                    focusable => false,
                    type => widget}.

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
