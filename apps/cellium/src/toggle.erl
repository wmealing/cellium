-module(toggle).
-export([render/1, render_focused/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => toggle,
                    on => false,
                    focusable => true,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    On = maps:get(on, Widget, false),
    Status = case On of true -> "ON "; false -> "OFF" end,
    ?TERMBOX:tb_print(X, Y, Fg, Bg, "< " ++ Status ++ " >"),
    ok.

-spec render_focused(map()) -> ok.
render_focused(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    On = maps:get(on, Widget, false),
    Status = case On of true -> "ON "; false -> "OFF" end,
    ?TERMBOX:tb_print(X, Y, Bg, Fg, "< " ++ Status ++ " >"),
    ok.
