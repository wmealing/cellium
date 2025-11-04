%%% @doc Table widget module for rendering tabular data with borders.
%%%
%%% This module provides functionality for rendering tables with configurable
%%% borders, headers, and rows. It supports various box styles and can render
%%% multi-column tables with different column widths.
%%% @end
-module(table).

-export([
    get_top/2,
    get_row/3,
    get_bottom/2,
    draw_table/7,
    render/1,
    draw_header/6,
    draw_bottom/6

]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @private
%%% @doc Builds a horizontal line segment for table borders.
-spec build_line(#box{}, [non_neg_integer()], string(), string(), string(), string()) -> iolist().
build_line(_Box, [], Left, _Horizontal, _Divider, Right) ->
    [Left, Right];
build_line(_Box, [Width], Left, Horizontal, _Divider, Right) ->
    [Left, lists:duplicate(Width, Horizontal), Right];
build_line(Box, [Width | Rest], Left, Horizontal, Divider, Right) ->
    [Left, lists:duplicate(Width, Horizontal), Divider | build_line(Box, Rest, "", Horizontal, Divider, Right)].

%%% @doc Generates the top border line of a table.
%%%
%%% @param Box The box style record defining border characters
%%% @param Widths List of column widths
%%% @returns A flattened iolist representing the top border
%%% @end
-spec get_top(#box{}, [non_neg_integer()]) -> string().
get_top(Box, Widths) ->
    lists:flatten(
      build_line(Box, Widths,
                 Box#box.top_left, Box#box.top,
                 Box#box.top_divider, Box#box.top_right)).

%%% @doc Generates a row separator line for a table.
%%%
%%% Different separator styles are available depending on the level:
%%% - `head': Header row separator
%%% - `row': Regular row separator
%%% - `mid': Middle separator (minimal)
%%% - `foot': Footer row separator
%%%
%%% @param Box The box style record defining border characters
%%% @param Level The type of row separator to generate
%%% @param Widths List of column widths
%%% @returns A flattened iolist representing the row separator
%%% @end
-spec get_row(#box{}, head | row | mid | foot, [non_neg_integer()]) -> string().
get_row(Box, Level, Widths) ->
    {Left, Horizontal, Cross, Right} = case Level of
        head -> {Box#box.head_row_left, Box#box.head_row_horizontal, Box#box.head_row_cross, Box#box.head_row_right};
        row ->  {Box#box.row_left,      Box#box.row_horizontal,      Box#box.row_cross,      Box#box.row_right};
        mid ->  {Box#box.mid_left,      " ",                         Box#box.mid_vertical,   Box#box.mid_right};
        foot -> {Box#box.foot_row_left, Box#box.foot_row_horizontal, Box#box.foot_row_cross, Box#box.foot_row_right}
    end,
    lists:flatten(build_line(Box, Widths, Left, Horizontal, Cross, Right)).

%%% @doc Generates the bottom border line of a table.
%%%
%%% @param Box The box style record defining border characters
%%% @param Widths List of column widths
%%% @returns A flattened iolist representing the bottom border
%%% @end
-spec get_bottom(#box{}, [non_neg_integer()]) -> string().
get_bottom(Box, Widths) ->
    lists:flatten(build_line(Box, Widths, Box#box.bottom_left, Box#box.bottom, Box#box.bottom_divider, Box#box.bottom_right)).

%%% @doc Draws the header line of a table.
%%%
%%% @param X Starting X coordinate
%%% @param Y Starting Y coordinate
%%% @param Fg Foreground color
%%% @param Bg Background color
%%% @param Box Box style to use
%%% @param ColumnWidths List of column widths
%%% @returns ok
%%% @end
-spec draw_header(integer(), integer(), atom(), atom(), #box{}, [non_neg_integer()]) -> ok.
draw_header(X, Y, Fg, Bg, Box, ColumnWidths) ->
    Line = get_top(Box, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line).

%%% @private
%%% @doc Recursively draws row separator lines.
-spec draw_rows(integer(), integer(), atom(), atom(), #box{}, non_neg_integer(), [non_neg_integer()]) -> ok.
draw_rows(_X, _Y, _Fg, _Bg, _Box, 0, _ColumnWidths) ->
    ok;
draw_rows(X, Y, Fg, Bg, Box, Height, ColumnWidths) ->
    Line = get_row(Box, mid, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line),
    draw_rows(X, Y + 1, Fg, Bg, Box, Height - 1, ColumnWidths).

%%% @doc Draws the bottom line of a table.
%%%
%%% @param X Starting X coordinate
%%% @param Y Starting Y coordinate
%%% @param Fg Foreground color
%%% @param Bg Background color
%%% @param Box Box style to use
%%% @param ColumnWidths List of column widths (wrapped in a list)
%%% @returns ok
%%% @end
-spec draw_bottom(integer(), integer(), atom(), atom(), #box{}, [[non_neg_integer()]]) -> ok.
draw_bottom(X, Y, Fg, Bg, Box, [ColumnWidths]) ->
    Line = get_bottom(Box, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line),
    ok.

%%% @doc Draws a complete table with borders.
%%%
%%% Renders a table frame consisting of header, row separators, and bottom.
%%% Does not render content, only the structural elements.
%%%
%%% @param X Starting X coordinate
%%% @param Y Starting Y coordinate
%%% @param Height Number of rows to draw
%%% @param Fg Foreground color
%%% @param Bg Background color
%%% @param Box Box style record
%%% @param ColumnWidths List of column widths
%%% @returns ok
%%% @end
-spec draw_table(integer(), integer(), integer(), atom(), atom(), #box{}, [non_neg_integer()]) -> ok.
draw_table(X,Y, Height,Fg,Bg,Box,ColumnWidths) ->
    draw_header(X, Y,          Fg, Bg, Box,         ColumnWidths),
    draw_rows(X,   Y + 1,      Fg, Bg, Box, Height, ColumnWidths),
    draw_bottom(X, Y + Height, Fg, Bg, Box,         [ColumnWidths]).

%%% @doc Renders a complete table widget with headers and data.
%%%
%%% Draws a full table including frame, optional headers, and data rows.
%%% Headers and rows are rendered using the table_row widget.
%%%
%%% @param Widget Table widget map containing position, dimensions, headers, and rows
%%% @returns ok
%%% @end
-spec render(map()) -> ok.
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


%%% @private
%%% @doc Iterates over the list of rows and renders each one.
-spec render_rows(integer(), integer(), atom(), atom(), [non_neg_integer()], [[string()]]) -> ok.
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
