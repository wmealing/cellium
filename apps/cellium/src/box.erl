%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(box).

% API
-export([render/1,  draw_box/4]).

-include("cellium.hrl").

render(Widget) ->
    X1 = maps:get(x, Widget),
    X2 = X1 + maps:get(width, Widget, 0),
    Y1 = maps:get(y, Widget, 0),
    Y2 = Y1 + maps:get(height, Widget, 0),
    draw_box(X1, Y1, X2, Y2).

draw_horizontal_line(Point, _, Point, Fg, Bg) ->
    ok;

draw_horizontal_line(X1,Y, X2, Fg, Bg) ->
    ?TERMBOX:tb_set_cell(X1, Y,  $─ , Fg, Bg),
    draw_horizontal_line(X1 + 1, Y, X2, Fg, Bg).

draw_vertical_line(Point, _, Point, _Fg, _Bg) ->
    ok;

draw_vertical_line(Y1, X, Y2, Fg, Bg) ->
    ?TERMBOX:tb_set_cell(X, Y1 + 1,  $│ , Fg, Bg),
    draw_vertical_line(Y1 + 1, X, Y2, Fg, Bg).


draw_box(X1, Y1, X2, Y2) ->
    #{bg := Bg, fg := Fg} = theme:load(box),

    draw_vertical_line(Y1, X2, Y2, Fg, Bg),
    draw_horizontal_line(X1 + 1, Y1, X2, Fg, Bg),
    draw_vertical_line(Y1, X1, Y2, Fg, Bg),
    draw_horizontal_line(X1 + 1, Y2, X2, Fg, Bg),
    ?TERMBOX:tb_set_cell(X1, Y1, $┌, Fg, Bg),
    ?TERMBOX:tb_set_cell(X2, Y1, $┐, Fg, Bg),
    ?TERMBOX:tb_set_cell(X1, Y2, $└, Fg, Bg),
    ?TERMBOX:tb_set_cell(X2, Y2, $┘, Fg, Bg),
    ok.

draw_words(_, Y, _, Y, []) ->
    ok;

draw_words(_, _, _, _, []) ->
    ok;

draw_words(X1, Y1, X2, Y2, Words) ->

    [FirstWord | Rest] =  Words,

    ?TERMBOX:tb_print(X1 +1,
                          Y1 +1,
                          ?TB_DEFAULT,
                          ?TB_DEFAULT, FirstWord),

    draw_words(X1,
               Y1 + 1,
               X2,
               Y2, Rest),
    ok.

lines_in_box(X1, Y1, X2, Y2, Lines) when is_list(Lines) ->
    draw_box(X1, Y1, X2, Y2),
    draw_words(X1, Y1, X2, Y2, Lines).
