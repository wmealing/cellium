%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(box_styles).

-include("cellium.hrl").

-export([
    ascii/0,
    ascii2/0,
    ascii_double_head/0,
    square/0,
    square_double_head/0,
    minimal/0,
    minimal_heavy_head/0,
    minimal_double_head/0,
    simple/0,
    simple_head/0,
    simple_heavy/0,
    horizontals/0,
    rounded/0,
    heavy/0,
    heavy_edge/0,
    heavy_head/0,
    double/0,
    double_edge/0,
    markdown/0,
    drawline/5,
    draw_horizontal_line/5,
    draw_vertical_line/5
]).




ascii() ->
    #box{
        top_left = "+", top = "-", top_divider = "-", top_right = "+",
        head_left = "|", head_vertical = " ", head_right = "|",
        head_row_left = "|", head_row_horizontal = "-", head_row_cross = "+", head_row_right = "|",
        mid_left = "|", mid_vertical = " ", mid_right = "|",
        row_left = "|", row_horizontal = "-", row_cross = "+", row_right = "|",
        foot_row_left = "|", foot_row_horizontal = "-", foot_row_cross = "+", foot_row_right = "|",
        foot_left = "|", foot_vertical = " ", foot_right = "|",
        bottom_left = "+", bottom = "-", bottom_divider = "-", bottom_right = "+",
        is_ascii = true
    }.

ascii2() ->
    #box{
        top_left = "+", top = "-", top_divider = "+", top_right = "+",
        head_left = "|", head_vertical = " ", head_right = "|",
        head_row_left = "+", head_row_horizontal = "-", head_row_cross = "+", head_row_right = "+",
        mid_left = "|", mid_vertical = " ", mid_right = "|",
        row_left = "+", row_horizontal = "-", row_cross = "+", row_right = "+",
        foot_row_left = "+", foot_row_horizontal = "-", foot_row_cross = "+", foot_row_right = "+",
        foot_left = "|", foot_vertical = " ", foot_right = "|",
        bottom_left = "+", bottom = "-", bottom_divider = "+", bottom_right = "+",
        is_ascii = true
    }.

ascii_double_head() ->
    #box{
        top_left = "+", top = "-", top_divider = "+", top_right = "+",
        head_left = "|", head_vertical = " ", head_right = "|",
        head_row_left = "=", head_row_horizontal = "=", head_row_cross = "+", head_row_right = "=",
        mid_left = "|", mid_vertical = " ", mid_right = "|",
        row_left = "+", row_horizontal = "-", row_cross = "+", row_right = "+",
        foot_row_left = "+", foot_row_horizontal = "-", foot_row_cross = "+", foot_row_right = "+",
        foot_left = "|", foot_vertical = " ", foot_right = "|",
        bottom_left = "+", bottom = "-", bottom_divider = "+", bottom_right = "+",
        is_ascii = true
    }.

square() ->
    #box{
        top_left = "┌", top = "─", top_divider = "┬", top_right = "┐",
        head_left = "│", head_vertical = " ", head_right = "│",
        head_row_left = "├", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "┤",
        mid_left = "│", mid_vertical = " ", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = " ", foot_right = "│",
        bottom_left = "└", bottom = "─", bottom_divider = "┴", bottom_right = "┘"
    }.

square_double_head() ->
    #box{
        top_left = "┌", top = "─", top_divider = "┬", top_right = "┐",
        head_left = "│", head_vertical = " ", head_right = "│",
        head_row_left = "╞", head_row_horizontal = "═", head_row_cross = "╪", head_row_right = "╡",
        mid_left = "│", mid_vertical = " ", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = " ", foot_right = "│",
        bottom_left = "└", bottom = "─", bottom_divider = "┴", bottom_right = "┘"
    }.

minimal() ->
    #box{
        top_left = " ", top = " ", top_divider = "╷", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = "│",
        head_row_left = "╶", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "╴",
        mid_left = " ", mid_vertical = " ", mid_right = "│",
        row_left = "╶", row_horizontal = "─", row_cross = "┼", row_right = "╴",
        foot_row_left = "╶", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "╴",
        foot_left = " ", foot_vertical = " ", foot_right = "│",
        bottom_left = " ", bottom = " ", bottom_divider = "╵", bottom_right = " "
    }.

minimal_heavy_head() ->
    #box{
        top_left = " ", top = " ", top_divider = "╷", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = "│",
        head_row_left = "╺", head_row_horizontal = "━", head_row_cross = "┿", head_row_right = "╸",
        mid_left = " ", mid_vertical = " ", mid_right = "│",
        row_left = "╶", row_horizontal = "─", row_cross = "┼", row_right = "╴",
        foot_row_left = "╶", foot_row_horizontal = "─", foot_row_right = "╴",
        foot_left = " ", foot_vertical = " ", foot_right = "│",
        bottom_left = " ", bottom = " ", bottom_divider = "╵", bottom_right = " "
    }.

minimal_double_head() ->
    #box{
        top_left = " ", top = " ", top_divider = "╷", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = "│",
        head_row_left = " ", head_row_horizontal = "═", head_row_cross = "╪", head_row_right = " ",
        mid_left = " ", mid_vertical = " ", mid_right = "│",
        row_left = " ", row_horizontal = "─", row_cross = "┼", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = " ",
        foot_left = " ", foot_vertical = " ", foot_right = "│",
        bottom_left = " ", bottom = " ", bottom_divider = "╵", bottom_right = " "
    }.

simple() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = " ",
        head_row_left = " ", head_row_horizontal = "─", head_row_cross = "─", head_row_right = " ",
        mid_left = " ", mid_vertical = " ", mid_right = " ",
        row_left = " ", row_horizontal = " ", row_cross = " ", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "─", foot_row_cross = "─", foot_row_right = " ",
        foot_left = " ", foot_vertical = " ", foot_right = " ",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " "
    }.

simple_head() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = " ",
        head_row_left = " ", head_row_horizontal = "─", head_row_cross = "─", head_row_right = " ",
        mid_left = " ", mid_vertical = " ", mid_right = " ",
        row_left = " ", row_horizontal = " ", row_cross = " ", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = " ", foot_row_cross = " ", foot_row_right = " ",
        foot_left = " ", foot_vertical = " ", foot_right = " ",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " "
    }.

simple_heavy() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = " ",
        head_row_left = " ", head_row_horizontal = "━", head_row_cross = "━", head_row_right = " ",
        mid_left = " ", mid_vertical = " ", mid_right = " ",
        row_left = " ", row_horizontal = " ", row_cross = " ", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "━", foot_row_cross = "━", foot_row_right = " ",
        foot_left = " ", foot_vertical = " ", foot_right = " ",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " "
    }.

horizontals() ->
    #box{
        top_left = " ", top = "─", top_divider = "─", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = " ",
        head_row_left = " ", head_row_horizontal = "─", head_row_cross = "─", head_row_right = " ",
        mid_left = " ", mid_vertical = " ", mid_right = " ",
        row_left = " ", row_horizontal = "─", row_cross = "─", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "─", foot_row_cross = "─", foot_row_right = " ",
        foot_left = " ", foot_vertical = " ", foot_right = " ",
        bottom_left = " ", bottom = "─", bottom_divider = "─", bottom_right = " "
    }.

rounded() ->
    #box{
        top_left = "╭", top = "─", top_divider = "┬", top_right = "╮",
        head_left = "│", head_vertical = " ", head_right = "│",
        head_row_left = "├", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "┤",
        mid_left = "│", mid_vertical = " ", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = " ", foot_right = "│",
        bottom_left = "╰", bottom = "─", bottom_divider = "┴", bottom_right = "╯"
    }.

heavy() ->
    #box{
        top_left = "┏", top = "━", top_divider = "┳", top_right = "┓",
        head_left = "┃", head_vertical = " ", head_right = "┃",
        head_row_left = "┣", head_row_horizontal = "━", head_row_cross = "╋", head_row_right = "┫",
        mid_left = "┃", mid_vertical = " ", mid_right = "┃",
        row_left = "┣", row_horizontal = "━", row_cross = "╋", row_right = "┫",
        foot_row_left = "┣", foot_row_horizontal = "━", foot_row_cross = "╋", foot_row_right = "┫",
        foot_left = "┃", foot_vertical = " ", foot_right = "┃",
        bottom_left = "┗", bottom = "━", bottom_divider = "┻", bottom_right = "┛"
    }.

heavy_edge() ->
    #box{
        top_left = "┏", top = "━", top_divider = "┯", top_right = "┓",
        head_left = "┃", head_vertical = " ", head_right = "┃",
        head_row_left = "┠", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "┨",
        mid_left = "┃", mid_vertical = " ", mid_right = "┃",
        row_left = "┠", row_horizontal = "─", row_cross = "┼", row_right = "┨",
        foot_row_left = "┠", foot_row_horizontal = "─", foot_row_right = "┨",
        foot_left = "┃", foot_vertical = " ", foot_right = "┃",
        bottom_left = "┗", bottom = "━", bottom_divider = "┷", bottom_right = "┛"
    }.

heavy_head() ->
    #box{
        top_left = "┏", top = "━", top_divider = "┳", top_right = "┓",
        head_left = "┃", head_vertical = " ", head_right = "┃",
        head_row_left = "┡", head_row_horizontal = "━", head_row_cross = "╇", head_row_right = "┩",
        mid_left = "│", mid_vertical = " ", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = " ", foot_right = "│",
        bottom_left = "└", bottom = "─", bottom_divider = "┴", bottom_right = "┘"
    }.

double() ->
    #box{
        top_left = "╔", top = "═", top_divider = "╦", top_right = "╗",
        head_left = "║", head_vertical = " ", head_right = "║",
        head_row_left = "╠", head_row_horizontal = "═", head_row_cross = "╬", head_row_right = "╣",
        mid_left = "║", mid_vertical = " ", mid_right = "║",
        row_left = "╠", row_horizontal = "═", row_cross = "╬", row_right = "╣",
        foot_row_left = "╠", foot_row_horizontal = "═", foot_row_cross = "╬", foot_row_right = "╣",
        foot_left = "║", foot_vertical = " ", foot_right = "║",
        bottom_left = "╚", bottom = "═", bottom_divider = "╩", bottom_right = "╝"
    }.

double_edge() ->
    #box{
        top_left = "╔", top = "═", top_divider = "╤", top_right = "╗",
        head_left = "║", head_vertical = " ", head_right = "║",
        head_row_left = "╟", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "╢",
        mid_left = "║", mid_vertical = " ", mid_right = "║",
        row_left = "╟", row_horizontal = "─", row_cross = "┼", row_right = "╢",
        foot_row_left = "╟", foot_row_horizontal = "─", foot_row_right = "╢",
        foot_left = "║", foot_vertical = " ", foot_right = "║",
        bottom_left = "╚", bottom = "═", bottom_divider = "╧", bottom_right = "╝"
    }.

markdown() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = "|", head_vertical = " ", head_right = "|",
        head_row_left = "|", head_row_horizontal = "-", head_row_cross = "|", head_row_right = "|",
        mid_left = "|", mid_vertical = " ", mid_right = "|",
        row_left = "|", row_horizontal = "-", row_cross = "|", row_right = "|",
        foot_row_left = "|",
        foot_left = "|", foot_vertical = " ", foot_right = "|",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " ",
        is_ascii = true
    }.

drawline(X, Y, Fg, Bg, Line) ->
    BinLine = unicode:characters_to_binary(Line),
    ?TERMBOX:tb_print(X,
                      Y,
                      Fg,
                      Bg,
                      BinLine
                      ).

draw_horizontal_line(X1, _Y, X2, _Bg, _Fg) when X1 > X2 ->
    ok;

draw_horizontal_line(X1, Y, X2, Bg, Fg) ->
    ?TERMBOX:tb_set_cell(X1, Y, $─, Bg, Fg),
    draw_horizontal_line(X1 + 1, Y, X2, Bg, Fg).

draw_vertical_line(Y1, _X, Y2, _Bg, _Fg) when Y1 > Y2 ->
    ok;

draw_vertical_line(Y1, X, Y2, Bg, Fg) ->
    ?TERMBOX:tb_set_cell(X, Y1, $│, Bg, Fg),
    draw_vertical_line(Y1 + 1, X, Y2, Bg, Fg).
