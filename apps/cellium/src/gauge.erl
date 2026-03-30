-module(gauge).
-export([render/2, render_focused/2, new/1]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => gauge,
                    type => widget,
                    value => 0,
                    label => <<>>,
                    focusable => true}.

render(Widget, Buffer) ->
    render_with_colors(Widget, false, Buffer).

render_focused(Widget, Buffer) ->
    render_with_colors(Widget, true, Buffer).

render_with_colors(Widget, Focused, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    {FinalFg, FinalBg} = case Focused of
        true -> {Bg, Fg};
        false -> {Fg, Bg}
    end,
    Value = maps:get(value, Widget, 0),
    Label = maps:get(label, Widget, <<>>),
    Width = min(maps:get(width, Widget, 10), maps:get(requested_width, Widget, maps:get(width, Widget, 10))),

    Buffer1 = cellium_buffer:put_string(X, Y, FinalFg, FinalBg, Label, Buffer),

    % Render progress bar
    BarWidth = Width - byte_size(Label) - 1,
    if
        BarWidth > 0 ->
            Filled = trunc(BarWidth * Value / 100),
            Buffer2 = lists:foldl(fun(I, Acc) ->
                cellium_buffer:set_cell(X + byte_size(Label) + 1 + I, Y, 16#2588, FinalFg, FinalBg, Acc) %% █
            end, Buffer1, lists:seq(0, max(0, Filled - 1))),
            lists:foldl(fun(I, Acc) ->
                cellium_buffer:set_cell(X + byte_size(Label) + 1 + I, Y, 16#2591, FinalFg, FinalBg, Acc) %% ░
            end, Buffer2, lists:seq(Filled, max(Filled, BarWidth - 1)));
        true ->
            Buffer1
    end.
