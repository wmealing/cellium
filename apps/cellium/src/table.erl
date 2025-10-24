-module(table).

-export([
    get_top/2,
    get_row/3,
    get_bottom/2,
    draw_table/7,
    render/1
]).

-include("cellium.hrl").

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

draw_header(X, Y, Fg, Bg, Box, ColumnWidths) ->
    Line = get_top(Box, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line).

draw_rows(_X, _Y, _Fg, _Bg, _Box, 0, _ColumnWidths) ->
    ok;

draw_rows(X, Y, Fg, Bg, Box, Height, ColumnWidths) ->
    Line = get_row(Box, mid, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line),
    draw_rows(X, Y + 1, Fg, Bg, Box, Height - 1, ColumnWidths).

draw_bottom(X, Y, Fg, Bg, Box, [ColumnWidths]) ->
    Line = get_bottom(Box, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line),
    ok.

draw_table(X,Y, Height,Fg,Bg,Box,ColumnWidths) ->

    draw_header(X, Y,          Fg, Bg, Box,         ColumnWidths),
    draw_rows(X,   Y + 1,      Fg, Bg, Box, Height, ColumnWidths),
    draw_bottom(X, Y + Height, Fg, Bg, Box,         [ColumnWidths]).


render(Widget) ->
    Bg = maps:get('background-color', Widget, black),
    Fg = maps:get(color, Widget, white),

    X = maps:get(x, Widget),
    Y = maps:get(y, Widget),

    Width =  maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    ColumnWidths = maps:get(column_widths, Widget, [Width -1]),

    Box = box_styles:double(),
    draw_table(X,Y, Height,Fg,Bg,Box,ColumnWidths),
    %% draw_header(X, Y, Fg, Bg, Box, ColumnWidths),
    %% draw_rows(X, Y + 1, Fg, Bg, Box, Height, ColumnWidths),
    %% draw_bottom(X, Y + Height, Fg, Bg, Box, [ColumnWidths]),
    ok.
