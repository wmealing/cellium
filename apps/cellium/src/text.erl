%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc Text Widget Module
%%%
%%% This module provides a text display widget that supports automatic
%%% word wrapping and multi-line text rendering. The text widget displays
%%% plain text content within specified dimensions, automatically wrapping
%%% words to fit the available width.
%%%
%%% Text is wrapped using a greedy word-wrap algorithm and rendered line
%%% by line within the height constraints of the widget.
%%% @end
%%% Created : 26 Sep 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(text).

%% API
-export([draw_lines_of_text/6, draw_line/5, render/1, new/2, render_focused/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc Creates a new text widget.
%%%
%%% Initializes a text widget with the given ID and content. The widget's
%%% initial dimensions are calculated based on the text length (width equals
%%% the byte size of the text, height defaults to 1).
%%%
%%% @param Id Unique identifier for the widget
%%% @param Words Binary text content to display
%%% @returns Widget map with text configuration
%%% @end
-spec new(term(), binary()) -> map().
new(Id, Words) ->
    (widget:new())#{id => Id,
                    type => widget,
                    widget_type => text,
                    value => Words,
                    width => bit_size(Words),
                    height => 1
}.

%%% @doc Renders the text widget to the terminal.
%%%
%%% Extracts widget rendering properties (position, colors, dimensions) and
%%% renders the text content with automatic word wrapping. Text is wrapped to
%%% fit the widget's width and rendered line by line up to the widget's height.
%%%
%%% @param Widget Map containing widget configuration
%%% @returns ok
%%% @end
-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    Words = maps:get(value, Widget, <<"NO TEXT">>),

    WrappedWords = greedy_wrap:word_wrap(Words, Width),
    draw_lines_of_text(X,Y,Fg,Bg, Height, WrappedWords),
    ok.

%%% @doc Renders the text widget to the terminal when it has focus.
%%%
%%% This function is similar to `render/1` but swaps the foreground and
%%% background colors to provide a visual indication that the widget is focused.
%%%
%%% @param Widget Map containing widget configuration
%%% @returns ok
%%% @end
-spec render_focused(map()) -> ok.
render_focused(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    Words = maps:get(value, Widget, <<"NO TEXT">>),

    WrappedWords = greedy_wrap:word_wrap(Words, Width),
    draw_lines_of_text(X,Y,Bg,Fg, Height, WrappedWords), %% Swapped Fg and Bg
    ok.


%%% @private
%%% @doc Draws a single line of text at the specified position.
%%%
%%% Renders a binary text string at the given terminal coordinates with
%%% the specified foreground and background colors.
%%%
%%% @param X X coordinate (column)
%%% @param Y Y coordinate (row)
%%% @param Fg Foreground color
%%% @param Bg Background color
%%% @param Word Binary text to display (empty list handled as base case)
%%% @returns ok
%%% @end
-spec draw_line(integer(), integer(), integer(), integer(), binary() | list()) -> ok.
draw_line(_X, _Y, _Fg, _Bg, []) ->
    ok;

draw_line(X,Y, Fg, Bg, Word) ->
    ?TERMBOX:tb_print(X,
                      Y,
                      Fg,
                      Bg,
                      Word).

%%% @private
%%% @doc Recursively draws multiple lines of text.
%%%
%%% Renders a list of wrapped text lines at successive Y coordinates,
%%% respecting the height limit. Empty lines are handled by incrementing
%%% the Y coordinate without rendering. Rendering stops when either the
%%% available space (height) is exhausted or all lines are rendered.
%%%
%%% @param X Starting X coordinate (column)
%%% @param Y Starting Y coordinate (row)
%%% @param Fg Foreground color
%%% @param Bg Background color
%%% @param Space Remaining vertical space (lines)
%%% @param Lines List of binary text lines to render
%%% @returns ok
%%% @end
-spec draw_lines_of_text(integer(), integer(), integer(), integer(), integer(), list()) -> ok.
%% terminate early if there is no 'lines' left.
draw_lines_of_text(_X, _Y,  _Fg, _Bg, 0, _l) ->
    ok;

%% terminate early if there is no content left.
draw_lines_of_text(_X, _Y,  _Fg, _Bg, _Space, []) ->
    ok;

%% Handle an empty line: just recurse, incrementing Y
draw_lines_of_text(X, Y, Fg, Bg, Space, [<<>> | Rest]) ->
    draw_lines_of_text(X, Y + 1, Fg, Bg, Space - 1, Rest);

%% Handle a line with content
draw_lines_of_text(X, Y, Fg, Bg, Space, [FirstLine | Rest]) ->
    draw_line(X, Y, Fg, Bg, FirstLine),
    draw_lines_of_text(X, Y + 1, Fg, Bg, Space - 1, Rest).
%%%===================================================================
%%% Internal functions
%%%===================================================================
