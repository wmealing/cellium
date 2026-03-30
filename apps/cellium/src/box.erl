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
-export([render/2, new/1, new/3]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @doc Creates a new box widget with default dimensions.
%%%
%%% @param Id Unique identifier for the box widget
%%% @returns A widget map configured as a box
%%% @end
-spec new(term()) -> map().
new(Id) ->
    new(Id, 0, 0).

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
                    orientation => vertical,
                    padding => #{top => 1, bottom => 1, left => 1, right => 1},
                    type => container }.

%%% @doc Renders the box widget to the terminal.
%%%
%%% Draws a bordered box at the widget's coordinates using the table
%%% drawing functions. The border style changes based on focus state:
%%% - Focused: double-line border
%%% - Unfocused: square border
%%%
%%% @param Widget The box widget map containing position and dimension info

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    HasFocus = maps:get(has_focus, Widget, false),
    HasChildFocus = has_child_focus(Widget),

    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    BoxStyle =
        case HasFocus orelse HasChildFocus of
            true ->
                box_styles:double();
            false ->
                box_styles:square()
        end,

    Width = min(maps:get(width, Widget, 0), maps:get(requested_width, Widget, maps:get(width, Widget, 0))),
    Height = min(maps:get(height, Widget, 0), maps:get(requested_height, Widget, maps:get(height, Widget, 0))),

    if
        Height > 0 andalso Width > 0 ->
            table:draw_table(X, Y, Height - 1, Fg, Bg, BoxStyle, [Width - 2], Buffer);
        true ->
            Buffer
    end.


has_child_focus(#{children := Children}) ->
    lists:any(fun(Child) -> 
        maps:get(focused, Child, false) orelse has_child_focus(Child)
    end, Children);
has_child_focus(_) ->
    false.
