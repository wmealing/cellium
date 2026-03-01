-module(editable_table).
-moduledoc """
An editable table widget with row selection and cell editing capabilities.

This widget extends the basic table functionality with navigation and editing.
Users can navigate through rows and cells, and edit cell values.
""".

-export([
    start/3,
    set_data/2,
    move_cursor/2,
    select_row/2,
    get_selected_row/1,
    start_edit/1,
    cancel_edit/1,
    commit_edit/1,
    render/1,
    handle_char_input/2,
    delete_char/1,
    render_focused/1
]).

-include("cellium.hrl").
-import(widget, [get_common_props/1, create/1]).

%% API Functions

-spec start(list(string()), list(list(string())), list(integer())) -> map().
start(Headers, InitialData, ColumnWidths) ->
    widget:create(
      (widget:new())#{type => editable_table,
                      data => InitialData,
                      headers => Headers,
                      column_widths => ColumnWidths,
                      cursor_row => 0,
                      cursor_col => 0,
                      mode => row_select,
                      edit_buffer => <<>>,
                      focusable => true
                     }).

set_data(State, NewData) ->
    State#{data => NewData}.

move_cursor(State, Direction) ->
    handle_navigation(State, Direction).

select_row(State, RowIndex) ->
    Data = maps:get(data, State),
    MaxRow = length(Data) - 1,
    ClampedRow = clamp(RowIndex, 0, MaxRow),
    State#{cursor_row => ClampedRow}.

get_selected_row(State) ->
    CursorRow = maps:get(cursor_row, State),
    Data = maps:get(data, State),
    case CursorRow < length(Data) of
        true -> lists:nth(CursorRow + 1, Data);
        false -> []
    end.

start_edit(State) ->
    CursorRow = maps:get(cursor_row, State),
    CursorCol = maps:get(cursor_col, State),
    Data = maps:get(data, State),
    Row = lists:nth(CursorRow + 1, Data),
    Cell = lists:nth(CursorCol + 1, Row),
    State#{mode => cell_edit, edit_buffer => Cell}.

cancel_edit(State) ->
    State#{mode => row_select, edit_buffer => <<>>}.

commit_edit(State) ->
    CursorRow = maps:get(cursor_row, State),
    CursorCol = maps:get(cursor_col, State),
    Data = maps:get(data, State),
    NewValue = maps:get(edit_buffer, State),

    UpdatedData = update_cell_value(Data, CursorRow, CursorCol, NewValue),
    State#{data => UpdatedData, mode => row_select, edit_buffer => <<>>}.

handle_char_input(State, Char) ->
    Mode = maps:get(mode, State),
    case Mode of
        cell_edit ->
            Buffer = maps:get(edit_buffer, State),
            NewBuffer = <<Buffer/binary, Char/binary>>,
            State#{edit_buffer => NewBuffer};
        _ ->
            State
    end.

delete_char(State) ->
    Mode = maps:get(mode, State),
    case Mode of
        cell_edit ->
            Buffer = maps:get(edit_buffer, State),
            case byte_size(Buffer) of
                0 -> State;
                Size ->
                    NewBuffer = binary:part(Buffer, 0, Size - 1),
                    State#{edit_buffer => NewBuffer}
            end;
        _ ->
            State
    end.

render(Widget) ->
    render_internal(Widget, false).

render_focused(Widget) ->
    render_internal(Widget, true).

render_internal(Widget, CursorVisible) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 40),
    Height = maps:get(height, Widget, 10),
    Style = maps:get(style, Widget, double),
    Box = box_styles:Style(),

    ColumnWidths = maps:get(column_widths, Widget, [Width - 1]),
    Headers = maps:get(headers, Widget, []),
    Data = maps:get(rows, Widget, maps:get(data, Widget, [])),
    CursorRow = maps:get(cursor_row, Widget, 0),
    Mode = maps:get(mode, Widget, row_select),
    CursorCol = maps:get(cursor_col, Widget, 0),
    EditBuffer = maps:get(edit_buffer, Widget, <<>>),

    %% Draw table frame
    table:draw_table(X, Y, Height, Fg, Bg, Box, ColumnWidths),

    %% Render headers
    HeaderOffset = render_headers(X, Y, Fg, Bg, Headers, ColumnWidths),

    %% Render data rows with selection
    render_data_rows(X, Y + HeaderOffset, Fg, Bg, ColumnWidths, Data, CursorRow, Mode, CursorCol, EditBuffer, CursorVisible),
    ok.

%% Internal Functions

handle_navigation(State, up) ->
    CursorRow = maps:get(cursor_row, State),
    NewRow = max(0, CursorRow - 1),
    State#{cursor_row => NewRow};

handle_navigation(State, down) ->
    CursorRow = maps:get(cursor_row, State),
    Data = maps:get(data, State),
    MaxRow = length(Data) - 1,
    NewRow = min(MaxRow, CursorRow + 1),
    State#{cursor_row => NewRow};

handle_navigation(State, left) ->
    Mode = maps:get(mode, State),
    case Mode of
        cell_edit ->
            CursorCol = maps:get(cursor_col, State),
            NewCol = max(0, CursorCol - 1),
            State#{cursor_col => NewCol};
        _ ->
            State
    end;

handle_navigation(State, right) ->
    Mode = maps:get(mode, State),
    case Mode of
        cell_edit ->
            CursorCol = maps:get(cursor_col, State),
            ColumnWidths = maps:get(column_widths, State),
            MaxCol = length(ColumnWidths) - 1,
            NewCol = min(MaxCol, CursorCol + 1),
            State#{cursor_col => NewCol};
        _ ->
            State
    end;

handle_navigation(State, _) ->
    State.

clamp(Value, Min, _Max) when Value < Min ->
    Min;
clamp(Value, _Min, Max) when Value > Max ->
    Max;
clamp(Value, _Min, _Max) ->
    Value.

update_cell_value(Data, RowIndex, ColIndex, NewValue) ->
    Row = lists:nth(RowIndex + 1, Data),
    UpdatedRow = update_list_at(Row, ColIndex, NewValue),
    update_list_at(Data, RowIndex, UpdatedRow).

update_list_at(List, Index, NewValue) ->
    {Before, [_Old | After]} = lists:split(Index, List),
    Before ++ [NewValue | After].

render_headers(_X, _Y, _Fg, _Bg, [], _ColumnWidths) ->
    1;
render_headers(X, Y, Fg, Bg, Headers, ColumnWidths) ->
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
    2.

render_data_rows(_X, _Y, _Fg, _Bg, _ColumnWidths, [], _CursorRow, _Mode, _CursorCol, _EditBuffer, _CursorVisible) ->
    ok;
render_data_rows(X, Y, Fg, Bg, ColumnWidths, Data, CursorRow, Mode, CursorCol, EditBuffer, CursorVisible) ->
    render_rows_with_index(X, Y, Fg, Bg, ColumnWidths, Data, 0, CursorRow, Mode, CursorCol, EditBuffer, CursorVisible).

render_rows_with_index(_X, _Y, _Fg, _Bg, _ColumnWidths, [], _Index, _CursorRow, _Mode, _CursorCol, _EditBuffer, _CursorVisible) ->
    ok;
render_rows_with_index(X, Y, Fg, Bg, ColumnWidths, [Row | Rest], Index, CursorRow, Mode, CursorCol, EditBuffer, CursorVisible) ->
    IsSelected = (Index =:= CursorRow),
    
    case {IsSelected, Mode} of
        {true, row_select} ->
            render_selected_row(X, Y, Fg, Bg, ColumnWidths, Row);
        {true, cell_edit} ->
            render_editing_row(X, Y, Fg, Bg, ColumnWidths, Row, CursorCol, EditBuffer, CursorVisible);
        {false, _} ->
            render_normal_row(X, Y, Fg, Bg, ColumnWidths, Row)
    end,
    
    render_rows_with_index(X, Y + 1, Fg, Bg, ColumnWidths, Rest, Index + 1, CursorRow, Mode, CursorCol, EditBuffer, CursorVisible).

render_normal_row(X, Y, Fg, Bg, ColumnWidths, Row) ->
    RowWidget = #{
        type => table_row,
        x => X,
        y => Y,
        row_data => Row,
        column_widths => ColumnWidths,
        color => Fg,
        'background-color' => Bg
    },
    table_row:render(RowWidget).

render_selected_row(X, Y, Fg, Bg, ColumnWidths, Row) ->
    RowWidget = #{
        type => table_row,
        x => X,
        y => Y,
        row_data => Row,
        column_widths => ColumnWidths,
        selected => true,
        color => Fg,
        'background-color' => Bg
    },
    table_row:render(RowWidget).

render_editing_row(X, Y, Fg, Bg, ColumnWidths, Row, CursorCol, EditBuffer, CursorVisible) ->
    render_cells_with_selection(X + 1, Y, Fg, Bg, ColumnWidths, Row, 0, CursorCol, EditBuffer, CursorVisible).

render_cells_with_selection(_X, _Y, _Fg, _Bg, [], [], _Index, _CursorCol, _EditBuffer, _CursorVisible) ->
    ok;
render_cells_with_selection(X, Y, Fg, Bg, [Width | RestWidths], [Cell | RestCells], Index, CursorCol, EditBuffer, CursorVisible) ->
    IsEditing = (Index =:= CursorCol),

    CellText = case IsEditing of
        true -> EditBuffer;
        false -> Cell
    end,

    CellWidget = #{
        type => table_cell,
        x => X,
        y => Y,
        width => Width,
        text => CellText,
        selected => IsEditing,
        color => Fg,
        'background-color' => Bg,
        cursor_pos => case {IsEditing, CursorVisible} of
                          {true, true} -> byte_size(EditBuffer);
                          _ -> -1
                      end
    },
    table_cell:render(CellWidget),

    render_cells_with_selection(X + Width + 1, Y, Fg, Bg, 
                                RestWidths, RestCells, Index + 1, 
                                CursorCol, EditBuffer, CursorVisible).

