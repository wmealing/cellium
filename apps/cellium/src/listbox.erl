%%% @doc Listbox widget module for displaying selectable lists.
%%%
%%% This module provides a listbox widget that renders a bordered container
%%% suitable for displaying a list of selectable items. The listbox uses
%%% rounded border styles and can track a selected item index.
%%% @end
-module(listbox).

-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @doc Creates a new listbox widget.
%%%
%%% Creates a listbox container with no padding. The listbox is designed
%%% to hold and display a list of items that can be selected.
%%%
%%% @param Id Unique identifier for the listbox
%%% @returns A widget map configured as a listbox container
%%% @end
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    type => container,
                    widget_type => listbox,
                    padding => #{top    => 0,
                                 bottom => 0,
                                 left   => 0,
                                 right  => 0}
                   }.

%%% @doc Renders the listbox widget with rounded borders.
%%%
%%% Draws the top and bottom borders of the listbox using rounded box
%%% style. The selected item index can be stored in the widget map but
%%% is not currently used in rendering.
%%%
%%% @param Widget The listbox widget map containing position and dimensions
%%% @returns ok
%%% @end
-spec render(map()) -> ok.
render(Widget) ->

    Box = box_styles:rounded(),
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    _Selected = maps:get(selected_idx, Widget, 0),
    Width = maps:get(width, Widget, 1),
    Height = maps:get(height, Widget, 1),

%    table:draw_table(X,Y, Height, Fg,Bg, Box, [Width -1]),
    table:draw_header(X, Y, Fg, Bg, Box, [Width -1]),
    table:draw_bottom(X, Y + Height, Fg, Bg, Box,[[Width -1]]),
    % okay so now we draw that table
    %% table:draw_header(X, Y, Fg, Bg, Box, Width),
    ok.
