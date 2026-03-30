-module(button).
-export([render/2, render_focused/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, "Button").

-spec new(term(), string()) -> map().
new(Id, Label) ->
    (widget:new())#{
        id => Id,
        widget_type => button,
        label => Label,
        class => button,
        focusable => true,
        type => widget
    }.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = widget:get_common_props(Widget),
    Label = maps:get(label, Widget, "Button"),
    HasFocus = maps:get(focused, Widget, false),
    Height = maps:get(height, Widget, 1),
    Width = maps:get(width, Widget, length(Label) + 2),

    {FinalFg, FinalBg} =
        case HasFocus of
            % Invert colors for focus
            true -> {Bg, Fg};
            false -> {Fg, Bg}
        end,

    if
        Height >= 3 ->
            BoxStyle =
                if
                    HasFocus -> box_styles:double();
                    true -> box_styles:square()
                end,
            % Draw the frame
            Buffer1 = table:draw_table(X, Y, Height - 1, FinalFg, FinalBg, BoxStyle, [Width - 2], Buffer),

            % Print label centered
            TextX = X + (Width - length(Label)) div 2,
            TextY = Y + Height div 2,
            cellium_buffer:put_string(TextX, TextY, FinalFg, FinalBg, Label, Buffer1);
        true ->
            FinalLabel =
                case HasFocus of
                    true -> "[" ++ Label ++ "]";
                    false -> " " ++ Label ++ " "
                end,
            cellium_buffer:put_string(X, Y, FinalFg, FinalBg, FinalLabel, Buffer)
    end.

-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    render(Widget, Buffer).
