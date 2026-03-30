-module(header).
-export([render/2, render_focused/2, new/2]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Id, Text) ->
    (widget:new())#{id => Id, widget_type => header, type => widget, text => Text}.

render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = maps:get(text, Widget, <<>>),
    cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer).

render_focused(Widget, Buffer) ->
    render(Widget, Buffer).
