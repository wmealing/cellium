%%% @doc Button widget module for rendering clickable buttons.
%%%
%%% This module provides a button widget that displays text centered within
%%% a bordered box. Buttons support callbacks that can be triggered on interaction.
%%% @end
-module(button).

-export([render/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @doc Creates a new button widget.
%%%
%%% Creates a button with the specified label text and callback function.
%%% The button will center its text within its bounds when rendered.
%%%
%%% @param Text The label text to display on the button
%%% @param Callback Function to be called when the button is activated
%%% @returns A widget map configured as a button
%%% @end
-spec new(binary() | string(), fun()) -> map().
new(Text, Callback) ->
    (widget:new())#{label => Text,
                    widget_type => button,
                    callback => Callback}.

%%% @doc Renders the button widget to the terminal.
%%%
%%% Draws a bordered box with centered text. The text is automatically
%%% centered both horizontally and vertically within the button's bounds.
%%%
%%% @param Widget The button widget map containing position, size, and label
%%% @returns ok
%%% @end
-spec render(map()) -> ok.
render(Widget) ->
    #{x := X1, y := Y1, fg := Fg, bg := Bg} = get_common_props(Widget),

    Style = maps:get(style, Widget, double),
    Box = box_styles:Style(),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    X2 = X1 + Width,
    Y2 = Y1 + Height,

    Label = maps:get(label, Widget, [<<"NO TEXT">>]),

    ButtonLength = X2 - X1,
    WordLength = string:length(Label),
    XOffset = trunc ((ButtonLength / 2) - (WordLength / 2) ),
    YOffset = trunc( (Y2 - Y1) / 2) -1 ,

    text:draw_lines_of_text(X1 + XOffset , Y1 + YOffset, Bg, Fg, 1, Label),
    table:draw_table(X1, Y1, X2 + Width, Fg, Bg, Box, [X2 - X1]).
