%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(box).

% API
-export([render/1,  draw_box/6]).

-include("cellium.hrl").

render(Widget) ->
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    X1 = maps:get(x, Widget),
    X2 = X1 + maps:get(width, Widget, 0),
    Y1 = maps:get(y, Widget, 0),
    Y2 = Y1 + maps:get(height, Widget, 0),
    draw_box(X1, Y1, X2, Y2, Bg, Fg).

draw_horizontal_line(Point, _, Point, _Bg, _Fg) ->
    ok;

draw_horizontal_line(X1,Y, X2, Bg, Fg) ->
    ?TERMBOX:tb_set_cell(X1, Y,  $─ , Bg, Fg),
    draw_horizontal_line(X1 + 1, Y, X2, Bg, Fg).

draw_vertical_line(Point, _, Point, _Bg, _Fg) ->
    ok;

draw_vertical_line(Y1, X, Y2, Bg, Fg) ->
    ?TERMBOX:tb_set_cell(X, Y1 + 1,  $│, Bg, Fg),
    draw_vertical_line(Y1 + 1, X, Y2, Bg, Fg).


draw_box(X1, Y1, X2, Y2, Bg, Fg) ->
    draw_vertical_line(Y1, X2, Y2, Bg, Fg),
    draw_horizontal_line(X1 + 1, Y1, X2, Bg, Fg),
    draw_vertical_line(Y1, X1, Y2, Bg, Fg),
    draw_horizontal_line(X1 + 1, Y2, X2, Bg, Fg),
    ?TERMBOX:tb_set_cell(X1, Y1, $┌, Bg, Fg),
    ?TERMBOX:tb_set_cell(X2, Y1, $┐, Bg, Fg),
    ?TERMBOX:tb_set_cell(X1, Y2, $└, Bg, Fg),
    ?TERMBOX:tb_set_cell(X2, Y2, $┘, Bg, Fg).


draw_words(_, Y, _, Y, _Bg, _Fg, []) ->
    ok;

draw_words(_, _, _, _, _Bg, _Fg, []) ->
    ok;

draw_words(X1, Y1, X2, Y2, Bg, Fg, Words) ->

    [FirstWord | Rest] =  Words,

    ?TERMBOX:tb_print(X1 +1,
                          Y1 +1,
                          Bg,
                          Fg, FirstWord),

    draw_words(X1,
               Y1 + 1,
               X2,
               Y2, Bg, Fg, Rest),
    ok.

lines_in_box(X1, Y1, X2, Y2, Bg, Fg, Lines) when is_list(Lines) ->
    draw_box(X1, Y1, X2, Y2, Bg, Fg),
    draw_words(X1, Y1, X2, Y2, Bg, Fg, Lines).
