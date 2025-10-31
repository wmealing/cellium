-module(table).

-export([
    get_top/2,
    get_row/3,
    get_bottom/2,
    draw_table/7,
    render/1
]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

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
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    Style = maps:get(style, Widget, double),
    Box = box_styles:Style(),

    ColumnWidths = maps:get(column_widths, Widget, [Width - 1]),

    %% 1. Draw the table frame and grid lines
    draw_table(X, Y, Height, Fg, Bg, Box, ColumnWidths),

    %% 2. Render the header row, if it exists
    Headers = maps:get(headers, Widget, []),
    HeaderOffset = case length(Headers) > 0 of
        true ->
            HeaderWidget = #{
                type => table_row,
                x => X,
                y => Y + 1,
                row_data => Headers,
                column_widths => ColumnWidths,
                color => Fg,
                'background-color' => Bg
            },
            table_row:render(HeaderWidget),
            2; % Rows start at Y+2
        false ->
            1  % Rows start at Y+1
    end,

    %% 3. Render the data rows
    Rows = maps:get(rows, Widget, []),
    render_rows(X, Y + HeaderOffset, Fg, Bg, ColumnWidths, Rows),
    ok.


%% --- Internal Helpers ---

%% @private
%% Iterates over the list of rows and renders each one.
render_rows(_X, _Y, _Fg, _Bg, _CW, []) -> ok;
render_rows(X, Y, Fg, Bg, CW, [Row | Rest]) ->
    RowWidget = #{
        type => table_row,
        x => X,
        y => Y,
        row_data => Row,
        column_widths => CW,
        color => Fg,
        'background-color' => Bg
    },
    table_row:render(RowWidget),
    render_rows(X, Y + 1, Fg, Bg, CW, Rest).
