-module(radio).
-export([render/1, render_focused/1, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, "Radio").

-spec new(term(), string()) -> map().
new(Id, Label) ->
    (widget:new())#{id => Id,
                    widget_type => radio,
                    label => Label,
                    selected => false,
                    focusable => true,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Label = maps:get(label, Widget, ""),
    Selected = maps:get(selected, Widget, false),
    Mark = case Selected of true -> "*"; false -> " " end,
    ?TERMBOX:tb_print(X, Y, Fg, Bg, "(" ++ Mark ++ ") " ++ Label),
    ok.

-spec render_focused(map()) -> ok.
render_focused(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Label = maps:get(label, Widget, ""),
    Selected = maps:get(selected, Widget, false),
    Mark = case Selected of true -> "*"; false -> " " end,
    ?TERMBOX:tb_print(X, Y, Bg, Fg, "(" ++ Mark ++ ") " ++ Label),
    ok.
