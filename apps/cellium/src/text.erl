-module(text).
-export([render/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, "").

-spec new(term(), string()) -> map().
new(Id, Content) ->
    (widget:new())#{id => Id,
                    widget_type => text,
                    text => Content,
                    type => widget}.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = maps:get(text, Widget, ""),
    cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer).
