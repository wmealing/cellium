-module(table_row).
-moduledoc """
A widget that renders a single row of a table.

This widget acts as a container. It takes a list of cell data (strings)
and a corresponding list of column widths. It then creates and renders a
`table_cell` widget for each piece of data, arranging them horizontally.
""".

-export([render/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-type widget() :: #{
    type := table_row,
    x := integer(),
    y := integer(),
    row_data := [string()],
    column_widths := [integer()],
    selected => boolean(),
    color => atom(),
    'background-color' => atom()
}.

-doc """
Renders a row of cells.

This function iterates through the `row_data` and `column_widths` lists,
creating and rendering a `table_cell` widget for each entry.

The `Widget` map is expected to have the following keys:
- `x`: The starting X coordinate for the entire row.
- `y`: The Y coordinate for the row.
- `row_data`: A list of strings, where each string is the content for a cell.
- `column_widths`: A list of integers representing the width of each cell.
- `color` (optional): The foreground color to be passed to the cells.
- `background-color` (optional): The background color to be passed to the cells.
""".
-spec render(widget()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    RowData = maps:get(row_data, Widget, []),
    ColumnWidths = maps:get(column_widths, Widget, []),
    IsSelected = maps:get(selected, Widget, false),

    render_cells(X, Y, Fg, Bg, ColumnWidths, RowData, IsSelected),
    ok.


%% --- Internal Helpers ---

%% @private
%% Iterates over data and widths, creating a cell for each.
render_cells(RowX, RowY, Fg, Bg, ColumnWidths, RowData, IsSelected) ->
    PairedData = lists:zip(RowData, ColumnWidths),
    lists:foldl(
        fun({CellText, CellWidth}, CurrentX) ->
            CellWidget = #{
                type => table_cell,
                x => CurrentX,
                y => RowY,
                width => CellWidth,
                text => CellText,
                selected => IsSelected,
                color => Fg,
                'background-color' => Bg
            },
            table_cell:render(CellWidget),
            CurrentX + CellWidth + 1 % +1 for the vertical line separator
        end,
        RowX + 1, % +1 to start drawing inside the left border of the table
        PairedData
    ).
