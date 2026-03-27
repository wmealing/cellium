-module(progress_bar).
-export([render/1, render_focused/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => progress_bar,
                    progress => 0.0, %% 0.0 to 1.0
                    width => 20,
                    focusable => true,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    render_with_colors(Widget, false).

-spec render_focused(map()) -> ok.
render_focused(Widget) ->
    render_with_colors(Widget, true).

render_with_colors(Widget, Focused) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    {FinalFg, FinalBg} = case Focused of
        true -> {Bg, Fg};
        false -> {Fg, Bg}
    end,
    Width = maps:get(requested_width, Widget, maps:get(width, Widget, 20)),
    Progress = maps:get(progress, Widget, 0.0),
    BarWidth = max(0, Width - 2),
    Filled = round(Progress * BarWidth),
    Bar = lists:duplicate(Filled, "█") ++ lists:duplicate(BarWidth - Filled, "░"),

    ?TERMBOX:tb_print(X, Y, FinalFg, FinalBg, "[" ++ Bar ++ "]"),
    ok.
