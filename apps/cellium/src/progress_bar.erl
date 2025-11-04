%%% @doc Progress bar widget module for displaying progress indicators.
%%%
%%% This module provides a visual progress bar widget that displays a percentage
%%% value using filled and empty character blocks. The bar automatically scales
%%% its filled portion based on the current percentage value.
%%% @end
-module(progress_bar).

-export([new/2, render/1, set_percentage/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @doc Creates a new progress bar widget.
%%%
%%% Creates a progress bar initialized with the given percentage value.
%%% The percentage is automatically clamped to the range 0-100.
%%%
%%% @param Id Unique identifier for the progress bar
%%% @param Percentage Initial progress value (0-100), will be clamped
%%% @returns A widget map configured as a progress bar
%%% @end
-spec new(term(), number()) -> map().
new(Id, Percentage) ->
    (widget:new())#{
        id => Id,
        type => widget,
        widget_type => progress_bar,
        value => clamp(Percentage),
        width => 22,
        height => 1
    }.

%%% @doc Renders the progress bar widget to the terminal.
%%%
%%% Displays a visual progress bar using block characters (█ for filled,
%%% ░ for empty). The bar shows 20 character positions representing the
%%% progress percentage.
%%%
%%% @param Widget The progress bar widget map
%%% @returns ok
%%% @end
-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    
    Percentage = maps:get(value, Widget, 0),
    
    Bar = build_bar(Percentage),
    
    ?TERMBOX:tb_print(X, Y, Fg, Bg, Bar),
    ok.

%%% @doc Updates the progress bar's percentage value.
%%%
%%% Sets a new percentage value for the progress bar, automatically
%%% clamping it to the valid range of 0-100.
%%%
%%% @param Widget The progress bar widget to update
%%% @param Percentage New progress value (0-100), will be clamped
%%% @returns Updated widget map
%%% @end
-spec set_percentage(map(), number()) -> map().
set_percentage(Widget, Percentage) ->
    Widget#{value => clamp(Percentage)}.

%%% @private
%%% @doc Clamps a value to the range 0-100.
-spec clamp(number()) -> 0..100.
clamp(V) when V < 0 ->
    0;
clamp(V) when V > 100 ->
    100;
clamp(V) ->
    V.

%%% @private
%%% @doc Builds the visual representation of the progress bar.
-spec build_bar(0..100) -> binary().
build_bar(Percentage) ->
    Filled = calculate_filled(Percentage),
    Empty = 20 - Filled,
    FilledChars = lists:duplicate(Filled, 9608),
    EmptyChars = lists:duplicate(Empty, 9617),
    AllChars = FilledChars ++ EmptyChars,
    unicode:characters_to_binary(AllChars).

%%% @private
%%% @doc Calculates how many character positions should be filled.
-spec calculate_filled(0..100) -> 0..20.
calculate_filled(Percentage) ->
    round((Percentage / 100) * 20).
