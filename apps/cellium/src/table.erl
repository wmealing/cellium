-module(table).

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
    get_top/2,
    get_row/3,
    get_bottom/2,
    render/1
]).

-include("cellium.hrl").

-record(box, {
    top_left, top, top_divider, top_right,
    head_left, head_vertical, head_right,
    head_row_left, head_row_horizontal, head_row_cross, head_row_right,
    mid_left, mid_vertical, mid_right,
    row_left, row_horizontal, row_cross, row_right,
    foot_row_left, foot_row_horizontal, foot_row_cross, foot_row_right,
    foot_left, foot_vertical, foot_right,
    bottom_left, bottom, bottom_divider, bottom_right,
    is_ascii = false
}).

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


build_line(_Box, [], Left, _Horizontal, _Divider, Right) ->
    [Left, Right];
build_line(_Box, [Width], Left, Horizontal, _Divider, Right) ->
    [Left, lists:duplicate(Width, Horizontal), Right];
build_line(Box, [Width | Rest], Left, Horizontal, Divider, Right) ->
    [Left, lists:duplicate(Width, Horizontal), Divider | build_line(Box, Rest, "", Horizontal, Divider, Right)].

get_top(Box, Widths) ->
    lists:flatten(
      build_line(Box, Widths,
                 Box#box.top_left, Box#box.top,
                 Box#box.top_divider, Box#box.top_right)).

get_row(Box, Level, Widths) ->
    {Left, Horizontal, Cross, Right} = case Level of
        head -> {Box#box.head_row_left, Box#box.head_row_horizontal, Box#box.head_row_cross, Box#box.head_row_right};
        row ->  {Box#box.row_left,      Box#box.row_horizontal,      Box#box.row_cross,      Box#box.row_right};
        mid ->  {Box#box.mid_left,      " ",                         Box#box.mid_vertical,   Box#box.mid_right};
        foot -> {Box#box.foot_row_left, Box#box.foot_row_horizontal, Box#box.foot_row_cross, Box#box.foot_row_right}
    end,
    lists:flatten(build_line(Box, Widths, Left, Horizontal, Cross, Right)).

get_bottom(Box, Widths) ->
    lists:flatten(build_line(Box, Widths, Box#box.bottom_left, Box#box.bottom, Box#box.bottom_divider, Box#box.bottom_right)).



drawline(X,Y,Fg,Bg, Line) ->
    BinLine = unicode:characters_to_binary(Line),
    ?TERMBOX:tb_print(X,
                      Y,
                      Fg,
                      Bg,
                      BinLine
                      ).

draw_header(X, Y, Fg, Bg, Box, ColumnWidths) ->
    Line = get_top(Box, ColumnWidths),
    drawline(X,Y,Fg,Bg, Line).

draw_rows(_X, _Y, _Fg, _Bg, _Box, 0, _ColumnWidths) ->
    ok;

draw_rows(X, Y, Fg, Bg, Box, Height, ColumnWidths) ->
    Line = get_row(Box, mid, ColumnWidths),
    drawline(X,Y,Fg,Bg, Line),
    draw_rows(X, Y + 1, Fg, Bg, Box, Height - 1, ColumnWidths).

draw_bottom(X, Y, Fg, Bg, Box, [ColumnWidths]) ->
    Line = get_bottom(Box, ColumnWidths),
    drawline(X,Y,Fg,Bg, Line),
    ok.

render(Widget) ->

%    Style = maps:get(style, Widget, double),
    Bg = maps:get('background-color', Widget, black),
    Fg = maps:get(color, Widget, white),

    X = maps:get(x, Widget),
    Y = maps:get(y, Widget),

    Width =  maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    ColumnWidths = maps:get(column_widths, Widget, [Width -1]),

    Box = table:double(),

    draw_header( X, Y, Fg, Bg, Box, ColumnWidths),
    draw_rows(X, Y + 1 , Fg, Bg, Box, Height, ColumnWidths),
    draw_bottom(X, Y + Height, Fg, Bg, Box, [ColumnWidths]),
    ok.
