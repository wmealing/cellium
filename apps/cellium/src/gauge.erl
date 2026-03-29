-module(gauge).
-export([render/1, render_focused/1, new/1]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => gauge,
                    type => widget,
                    value => 0,
                    label => <<>>,
                    focusable => true}.

render(Widget) ->
    render_with_colors(Widget, false).

render_focused(Widget) ->
    render_with_colors(Widget, true).

render_with_colors(Widget, Focused) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    {FinalFg, FinalBg} = case Focused of
        true -> {Bg, Fg};
        false -> {Fg, Bg}
    end,
    Value = maps:get(value, Widget, 0),
    Label = maps:get(label, Widget, <<>>),
    Width = min(maps:get(width, Widget, 10), maps:get(requested_width, Widget, maps:get(width, Widget, 10))),

    ?TERMBOX:tb_print(X, Y, FinalFg, FinalBg, Label),

    % Render progress bar
    BarWidth = Width - byte_size(Label) - 1,
    if
        BarWidth > 0 ->
            Filled = trunc(BarWidth * Value / 100),
            [
                ?TERMBOX:tb_set_cell(X + byte_size(Label) + 1 + I, Y, 16#2588, FinalFg, FinalBg) %% █
             || I <- lists:seq(0, Filled - 1)
            ],
            [
                ?TERMBOX:tb_set_cell(X + byte_size(Label) + 1 + I, Y, 16#2591, FinalFg, FinalBg) %% ░
             || I <- lists:seq(Filled, BarWidth - 1)
            ],
            ok;
        true ->
            ok
    end.
