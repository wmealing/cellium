-module(button).
-moduledoc """
Button widget module for interactive clickable buttons.

Buttons are focusable widgets that can be activated with Enter or Space.
When focused, they display with inverted colors or a border to indicate selection.

## Usage

```
button:new(submit_btn, "Submit")
```

## Properties

- `label` (string): The text displayed on the button
- `focusable` (boolean): Always true for buttons
- `focused` (boolean): Set by focus manager when button has focus
- `height` (integer): If >= 3, renders with a box border

## Rendering

- Height < 3: Simple text with brackets when focused: `[Submit]`
- Height >= 3: Bordered box with centered label (double border when focused)
- Focused state inverts foreground/background colors
""".

-export([render/2, render_focused/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new button widget with default label 'Button'.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, "Button").

-doc "Creates a new button widget with the specified label.".
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

-doc "Renders the button widget to the buffer.".
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
