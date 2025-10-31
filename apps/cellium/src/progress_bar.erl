-module(progress_bar).

-export([new/2, render/1, set_percentage/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Id, Percentage) ->
    (widget:new())#{
        id => Id,
        type => widget,
        widget_type => progress_bar,
        value => clamp(Percentage),
        width => 22,
        height => 1
    }.

render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    
    Percentage = maps:get(value, Widget, 0),
    
    Bar = build_bar(Percentage),
    
    ?TERMBOX:tb_print(X, Y, Fg, Bg, Bar),
    ok.

set_percentage(Widget, Percentage) ->
    Widget#{value => clamp(Percentage)}.

clamp(V) when V < 0 ->
    0;
clamp(V) when V > 100 ->
    100;
clamp(V) ->
    V.

build_bar(Percentage) ->
    Filled = calculate_filled(Percentage),
    Empty = 20 - Filled,
    FilledChars = lists:duplicate(Filled, 9608),
    EmptyChars = lists:duplicate(Empty, 9617),
    AllChars = FilledChars ++ EmptyChars,
    unicode:characters_to_binary(AllChars).

calculate_filled(Percentage) ->
    round((Percentage / 100) * 20).
