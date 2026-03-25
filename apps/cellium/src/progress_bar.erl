-module(progress_bar).
-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => progress_bar,
                    progress => 0.0, %% 0.0 to 1.0
                    width => 20,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Width = maps:get(width, Widget, 20),
    Progress = maps:get(progress, Widget, 0.0),
    Filled = round(Progress * Width),
    Bar = lists:duplicate(Filled, "#") ++ lists:duplicate(Width - Filled, "."),
    ?TERMBOX:tb_print(X, Y, Fg, Bg, "[" ++ Bar ++ "]"),
    ok.
