-module(button).
-export([render/1, render_focused/1, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, "Button").

-spec new(term(), string()) -> map().
new(Id, Label) ->
    (widget:new())#{id => Id,
                    widget_type => button,
                    label => Label,
                    class => button,
                    focusable => true,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = widget:get_common_props(Widget),
    Label = maps:get(label, Widget, "Button"),
    HasFocus = maps:get(has_focus, Widget, false),
    Height = maps:get(height, Widget, 1),
    Width = maps:get(width, Widget, length(Label) + 2),

    {FinalFg, FinalBg} = case HasFocus of
        true -> {Bg, Fg}; % Invert colors for focus
        false -> {Fg, Bg}
    end,

    if Height >= 3 ->
        BoxStyle = if HasFocus -> box_styles:double(); true -> box_styles:square() end,
        % Draw the frame
        table:draw_table(X, Y, Height - 1, FinalFg, FinalBg, BoxStyle, [Width - 2]),
        
        % Print label centered
        TextX = X + (Width - length(Label)) div 2,
        TextY = Y + Height div 2,
        ?TERMBOX:tb_print(TextX, TextY, FinalFg, FinalBg, Label),
        ok;
    true ->
        FinalLabel = case HasFocus of
            true -> "[" ++ Label ++ "]";
            false -> " " ++ Label ++ " "
        end,
        ?TERMBOX:tb_print(X, Y, FinalFg, FinalBg, FinalLabel),
        ok
    end.

-spec render_focused(map()) -> ok.
render_focused(Widget) ->
    render(Widget).
