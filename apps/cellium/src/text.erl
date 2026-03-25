-module(text).
-export([render/1, new/1, new/2]).

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

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = maps:get(text, Widget, ""),
    ?TERMBOX:tb_print(X, Y, Fg, Bg, Text),
    ok.
