-module(checkbox).
-export([render/1, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, "Checkbox").

-spec new(term(), string()) -> map().
new(Id, Label) ->
    (widget:new())#{id => Id,
                    widget_type => checkbox,
                    label => Label,
                    checked => false,
                    focusable => true,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Label = maps:get(label, Widget, ""),
    Checked = maps:get(checked, Widget, false),
    Mark = case Checked of true -> "X"; false -> " " end,
    ?TERMBOX:tb_print(X, Y, Fg, Bg, "[" ++ Mark ++ "] " ++ Label),
    ok.
