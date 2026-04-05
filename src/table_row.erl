%%% @doc Table row widget for rendering individual rows within a table.
%%%
%%% This module renders the data cells of a table row, distributing content
%%% across columns with the specified widths.
%%% @end
-module(table_row).

-export([render/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @doc Renders a table row with data cells.
%%%
%%% @param Widget Table row widget map containing row_data and column_widths
%%% @param Buffer Current frame buffer
%%% @returns Updated buffer
%%% @end
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    RowData = maps:get(row_data, Widget, []),
    ColumnWidths = maps:get(column_widths, Widget, []),

    render_cells(X, Y, Fg, Bg, RowData, ColumnWidths, Buffer).

%%% @private
%%% @doc Renders each cell in the row at the appropriate column position.
-spec render_cells(integer(), integer(), atom(), atom(), [string()], [non_neg_integer()], map()) -> map().
render_cells(_X, _Y, _Fg, _Bg, [], _, Buffer) ->
    Buffer;
render_cells(_X, _Y, _Fg, _Bg, _, [], Buffer) ->
    Buffer;
render_cells(X, Y, Fg, Bg, [Cell | RestCells], [Width | RestWidths], Buffer) ->
    % Prepare cell content (truncate or pad to fit column width)
    CellText = lists:flatten(io_lib:format("~s", [Cell])),
    CellLen = string:length(CellText),

    Content = if
        CellLen > Width ->
            % Truncate if too long
            string:slice(CellText, 0, Width);
        CellLen < Width ->
            % Pad with spaces if too short
            CellText ++ lists:duplicate(Width - CellLen, $ );
        true ->
            CellText
    end,

    % The table frame already drew borders, we start after the left border (X+1)
    % and place content there
    Buffer1 = cellium_buffer:put_string(X + 1, Y, Fg, Bg, Content, Buffer),

    % Move to the next column position
    % Each column takes: 1 (left border) + Width (content) = Width + 1
    NextX = X + Width + 1,
    render_cells(NextX, Y, Fg, Bg, RestCells, RestWidths, Buffer1).
