-module(toggle).
-export([render/2, render_focused/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => toggle,
                    on => false,
                    focusable => true,
                    type => widget}.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    On = maps:get(on, Widget, false),
    Status = case On of true -> "ON "; false -> "OFF" end,
    cellium_buffer:put_string(X, Y, Fg, Bg, "< " ++ Status ++ " >", Buffer).

-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    On = maps:get(on, Widget, false),
    Status = case On of true -> "ON "; false -> "OFF" end,
    cellium_buffer:put_string(X, Y, Bg, Fg, "< " ++ Status ++ " >", Buffer).
