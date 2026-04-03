%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(tab).
-moduledoc """
Tab widget module for rendering tabbed containers.

This module provides a container widget that displays a tab bar integrated
into the top border of the container with a "popped-up" cap for each tab.
""".

% API
-export([render/2, new/1, new/3]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new tab widget with default properties.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, 0, 0).

-doc "Creates a new tab widget with specified dimensions.".
-spec new(term(), non_neg_integer(), non_neg_integer()) -> map().
new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => tab,
                    width => Width,
                    height => Height,
                    tabs => [],
                    active_tab => 0,
                    focusable => true,
                    % top => 2 because the tab header now takes 2 rows (Y and Y+1)
                    padding => #{top => 2, bottom => 1, left => 1, right => 1},
                    type => container}.

-doc """
Renders the integrated tab bar with caps and the content area.
""".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),
    Tabs = maps:get(tabs, Widget, []),
    ActiveIdx = maps:get(active_tab, Widget, 0),

    if
        Height > 1 andalso Width > 0 ->
            Style = box_styles:square(),
            % 1. Draw the main box starting at Y+1
            % We use Height-1 because the header (tabs) starts at Y, but the box frame is at Y+1
            Buffer1 = table:draw_table(X, Y + 1, Height - 2, Fg, Bg, Style, [Width - 2], Buffer),

            % 2. Draw the tabs popping up from the top border line (Y and Y+1)
            draw_popped_tabs(X, Y, Width, Tabs, ActiveIdx, Fg, Bg, Buffer1);
        true ->
            Buffer
    end.

draw_popped_tabs(X, Y, TotalWidth, Tabs, ActiveIdx, Fg, Bg, Buffer) ->
    % Initial offset from the left corner of the main box
    % Main box top-left corner is at (X, Y+1)
    InitialOffset = 3,
    MaxX = X + TotalWidth - 2,

    {FinalBuffer, _} = lists:foldl(fun({Idx, Label}, {AccBuffer, CurrentX}) ->
        IsActive = (Idx == ActiveIdx),
        LabelLen = length(Label),
        TabWidth = LabelLen + 2,

        if (CurrentX + TabWidth) =< MaxX ->
            % Row Y: The Cap ┌──────┐
            B1 = cellium_buffer:set_cell(CurrentX, Y, $┌, Fg, Bg, AccBuffer),
            B2 = draw_h_line(CurrentX + 1, Y, LabelLen, $─, Fg, Bg, B1),
            B3 = cellium_buffer:set_cell(CurrentX + LabelLen + 1, Y, $┐, Fg, Bg, B2),

            % Row Y+1: The Sides │Label│ integrated into the horizontal line
            % We highlight the active tab's label
            {LFg, LBg} = if IsActive -> {Bg, Fg}; true -> {Fg, Bg} end,

            B4 = cellium_buffer:set_cell(CurrentX, Y + 1, $│, Fg, Bg, B3),
            B5 = cellium_buffer:put_string(CurrentX + 1, Y + 1, LFg, LBg, Label, B4),
            B6 = cellium_buffer:set_cell(CurrentX + LabelLen + 1, Y + 1, $│, Fg, Bg, B5),

            {B6, CurrentX + TabWidth + 1};
        true ->
            {AccBuffer, CurrentX}
        end
    end, {Buffer, X + InitialOffset}, lists:zip(lists:seq(0, length(Tabs) - 1), Tabs)),

    FinalBuffer.

draw_h_line(X, Y, Width, Char, Fg, Bg, Buffer) ->
    lists:foldl(fun(CX, Acc) ->
        cellium_buffer:set_cell(CX, Y, Char, Fg, Bg, Acc)
    end, Buffer, lists:seq(X, X + Width - 1)).
