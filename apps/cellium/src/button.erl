%%% @doc Button widget module for rendering clickable buttons.
%%%
%%% This module provides a button widget that displays text centered within
%%% a bordered box. Buttons support callbacks that can be triggered on interaction.
%%% @end
-module(button).

-export([render/1, new/2, render_focused/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1, create/1]).

%%% @doc Creates a new button widget.
%%%
%%% Creates a button with the specified label text and callback function.
%%% The button will center its text within its bounds when rendered.
%%%
%%% @param Text The label text to display on the button
%%% @param Callback Function to be called when the button is activated
%%% @returns A widget map configured as a button
%%% @end
-spec new(term(), binary() | string()) -> map().
new(Id, Label) ->
    widget:create(
      (widget:new())#{id => Id,
                      label => Label,
                      type => widget,
                      widget_type => button,
                      focusable => true
                     }).

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

    Id = maps:get(id, Widget, none),

    logger:info("NORMAL BUTTON RENDERING ~p",[Id]),
    #{x := X1, y := Y1, fg := Fg, bg := Bg} = get_common_props(Widget),

    Style = maps:get(style, Widget, double),
    Box = box_styles:Style(),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    X2 = X1 + Width - 2,
    Y2 = Y1 + Height - 1,

    Label = maps:get(label, Widget, <<"NO TEXT">>),

    ButtonLength = X2 - X1,
    WordLength = byte_size(Label),
    XOffset = trunc ((ButtonLength / 2) - (WordLength / 2) ),
    YOffset = trunc( (Y2 - Y1) / 2),

    table:draw_table(X1, Y1, Y2 - Y1, Fg, Bg, Box, [X2 - X1]),
    text:draw_line(X1 + 1 + XOffset , Y1 + YOffset, Fg, Bg, Label).

%%% @doc Renders the button widget to the terminal when it has focus.
%%%
%%% This function is similar to `render/1` but swaps the foreground and
%%% background colors to provide a visual indication that the widget is focused.
%%%
%%% @param Widget The button widget map containing position, size, and label
%%% @returns ok
%%% @end
-spec render_focused(map()) -> ok.
render_focused(Widget) ->

    logger:info("BUTTON FOCUSED"),
    #{x := X1, y := Y1, fg := Fg, bg := Bg} = get_common_props(Widget),

    Style = maps:get(style, Widget, double),
    Box = box_styles:Style(),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    X2 = X1 + Width - 2,
    Y2 = Y1 + Height - 1,

    Label = maps:get(label, Widget, <<"NO TEXT">>),

    ButtonLength = X2 - X1,
    WordLength = byte_size(Label),
    XOffset = trunc ((ButtonLength / 2) - (WordLength / 2) ),
    YOffset = trunc( (Y2 - Y1) / 2),

    table:draw_table(X1, Y1, Y2 - Y1, Bg, Fg, Box, [X2 - X1]),
    text:draw_line(X1 + 1 + XOffset , Y1 + YOffset, Bg, Fg, Label).

