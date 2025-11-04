%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%% @doc Box widget module for rendering rectangular boxes with borders.
%%%
%%% This module provides a simple box widget that can be rendered with
%%% different border styles depending on focus state. The box uses the
%%% table drawing functionality to render its borders.
%%% @end
-module(box).

% API
-export([render/1, new/3]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @doc Creates a new box widget.
%%%
%%% Creates a rectangular box widget with the specified dimensions.
%%% The box can be rendered with different border styles and colors.
%%%
%%% @param Id Unique identifier for the box widget
%%% @param Width Width of the box in characters
%%% @param Height Height of the box in characters
%%% @returns A widget map configured as a box
%%% @end
-spec new(term(), non_neg_integer(), non_neg_integer()) -> map().
new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => box,
                    width => Width,
                    height => Height,
                    type => widget }.

%%% @doc Renders the box widget to the terminal.
%%%
%%% Draws a bordered box at the widget's coordinates using the table
%%% drawing functions. The border style changes based on focus state:
%%% - Focused: double-line border
%%% - Unfocused: square border
%%%
%%% @param Widget The box widget map containing position and dimension info
%%% @returns ok
%%% @end
-spec render(map()) -> ok.
render(Widget) ->
    HasFocus = maps:get(has_focus, Widget, false),

    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    BoxStyle =
        case HasFocus of
            true ->
                box_styles:double();
            false ->
                box_styles:square()
        end,

    Height = maps:get(height,Widget, 0),
    Width = maps:get(width, Widget, 0),

    table:draw_table(X, Y, Height - 1, Fg,Bg, BoxStyle, [Width - 2]),
    ok.
