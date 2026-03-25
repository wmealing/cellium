-module(header).
-export([render/1, render_focused/1, new/2]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Id, Text) ->
    (widget:new())#{id => Id, widget_type => header, type => widget, text => Text}.

render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = maps:get(text, Widget, <<>>),
    ?TERMBOX:tb_print(X, Y, Fg, Bg, Text),
    ok.

render_focused(Widget) ->
    render(Widget).
