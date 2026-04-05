-module(table).
-moduledoc """
  The table widget module provides functionality for rendering tabular data
  with borders, headers, and rows. It supports various box styles and can
  render multi-column tables with configurable column widths.
""".

-export([
    new/1,
    new/3,
    get_top/2,
    get_row/3,
    get_bottom/2,
    draw_table/8,
    render/2,
    render_focused/2,
    draw_header/7,
    draw_bottom/7,
    handle_event/2

]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new table widget with default dimensions (0x0).".
-spec new(term()) -> map().
new(Id) ->
    new(Id, 0, 0).

-doc """
  Creates a new table widget with the specified identifier, width, and height.

  Parameters:
  - `Id`: A unique identifier for the widget.
  - `Width`: Initial width of the table.
  - `Height`: Initial height of the table.
""".
-spec new(term(), non_neg_integer(), non_neg_integer()) -> map().
new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => table,
                    width => Width,
                    height => Height,
                    column_widths => [Width],
                    headers => [],
                    rows => [],
                    editable => false,
                    focusable => false,
                    selected_row => 0,
                    selected_col => 0,
                    editing => false,
                    edit_text => "",
                    edit_cursor_pos => 0,
                    type => widget}.

%% @private
-doc "Builds a horizontal line segment for table borders.".
-spec build_line(#box{}, [non_neg_integer()], string(), string(), string(), string()) -> iolist().
build_line(_Box, [], Left, _Horizontal, _Divider, Right) ->
    [Left, Right];
build_line(_Box, [Width], Left, _Horizontal, _Divider, Right) when Width =< 0 ->
    [Left, "", Right];
build_line(_Box, [Width], Left, Horizontal, _Divider, Right) ->
    [Left, lists:duplicate(Width, Horizontal), Right];
build_line(Box, [Width | Rest], Left, Horizontal, Divider, Right) when Width =< 0 ->
    [Left, "", Divider | build_line(Box, Rest, "", Horizontal, Divider, Right)];
build_line(Box, [Width | Rest], Left, Horizontal, Divider, Right) ->
    [Left, lists:duplicate(Width, Horizontal), Divider | build_line(Box, Rest, "", Horizontal, Divider, Right)].

-doc """
Generates the top border line of a table.

- `Box`: The box style record defining border characters
- `Widths`: List of column widths
- Returns: A flattened iolist representing the top border
""".
-spec get_top(#box{}, [non_neg_integer()]) -> string().
get_top(Box, Widths) ->
    lists:flatten(
      build_line(Box, Widths,
                 Box#box.top_left, Box#box.top,
                 Box#box.top_divider, Box#box.top_right)).

-doc """
Generates a row separator line for a table.

Different separator styles are available depending on the level:
- `head`: Header row separator
- `row`: Regular row separator
- `mid`: Middle separator (minimal)
- `foot`: Footer row separator

- `Box`: The box style record defining border characters
- `Level`: The type of row separator to generate
- `Widths`: List of column widths
- Returns: A flattened iolist representing the row separator
""".
-spec get_row(#box{}, head | row | mid | foot, [non_neg_integer()]) -> string().
get_row(Box, Level, Widths) ->
    {Left, Horizontal, Cross, Right} = case Level of
        head -> {Box#box.head_row_left, Box#box.head_row_horizontal, Box#box.head_row_cross, Box#box.head_row_right};
        row ->  {Box#box.row_left,      Box#box.row_horizontal,      Box#box.row_cross,      Box#box.row_right};
        mid ->  {Box#box.mid_left,      " ",                         Box#box.mid_vertical,   Box#box.mid_right};
        foot -> {Box#box.foot_row_left, Box#box.foot_row_horizontal, Box#box.foot_row_cross, Box#box.foot_row_right}
    end,
    lists:flatten(build_line(Box, Widths, Left, Horizontal, Cross, Right)).

-doc """
Generates the bottom border line of a table.

- `Box`: The box style record defining border characters
- `Widths`: List of column widths
- Returns: A flattened iolist representing the bottom border
""".
-spec get_bottom(#box{}, [non_neg_integer()]) -> string().
get_bottom(Box, Widths) ->
    lists:flatten(build_line(Box, Widths, Box#box.bottom_left, Box#box.bottom, Box#box.bottom_divider, Box#box.bottom_right)).

-doc """
  Draws the header line of a table.

  - `X`: Starting X coordinate
  - `Y`: Starting Y coordinate
  - `Fg`: Foreground color
  - `Bg`: Background color
  - `Box`: Box style to use
  - `ColumnWidths`: List of column widths
""".
-spec draw_header(integer(), integer(), atom(), atom(), #box{}, [non_neg_integer()], map()) -> map().
draw_header(X, Y, Fg, Bg, Box, ColumnWidths, Buffer) ->
    Line = get_top(Box, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line, Buffer).

%% @private
-doc "Recursively draws row separator lines.".
-spec draw_rows(integer(), integer(), atom(), atom(), #box{}, non_neg_integer(), [non_neg_integer()], map()) -> map().
draw_rows(_X, _Y, _Fg, _Bg, _Box, 0, _ColumnWidths, Buffer) ->
    Buffer;
draw_rows(X, Y, Fg, Bg, Box, Height, ColumnWidths, Buffer) ->
    Line = get_row(Box, mid, ColumnWidths),
    Buffer1 = box_styles:drawline(X, Y, Fg, Bg, Line, Buffer),
    draw_rows(X, Y + 1, Fg, Bg, Box, Height - 1, ColumnWidths, Buffer1).

-doc """
  Draws the bottom line of a table.

  - `X`: Starting X coordinate
  - `Y`: Starting Y coordinate
  - `Fg`: Foreground color
  - `Bg`: Background color
  - `Box`: Box style to use
  - `ColumnWidths`: List of column widths
  - `Buffer`: Current frame buffer
  - Returns: Updated buffer
""".
-spec draw_bottom(integer(), integer(), atom(), atom(), #box{}, [non_neg_integer()], map()) -> map().
draw_bottom(X, Y, Fg, Bg, Box, ColumnWidths, Buffer) ->
    Line = get_bottom(Box, ColumnWidths),
    box_styles:drawline(X, Y, Fg, Bg, Line, Buffer).

-doc """
  Draws a complete table with borders.

  Renders a table frame consisting of header, row separators, and bottom.
  Does not render content, only the structural elements.

  - `X`: Starting X coordinate
  - `Y`: Starting Y coordinate
  - `Height`: Number of rows to draw
  - `Fg`: Foreground color
  - `Bg`: Background color
  - `Box`: Box style record
  - `ColumnWidths`: List of column widths
  - `Buffer`: Current frame buffer
  - Returns: Updated buffer
""".
-spec draw_table(integer(), integer(), integer(), atom(), atom(), #box{}, [non_neg_integer()], map()) -> map().
draw_table(X,Y, Height,Fg,Bg,Box,ColumnWidths, Buffer) ->
    Buffer1 = draw_header(X, Y,          Fg, Bg, Box,         ColumnWidths, Buffer),
    Buffer2 = draw_rows(X,   Y + 1,      Fg, Bg, Box, max(0, Height - 2), ColumnWidths, Buffer1),
    draw_bottom(X, Y + max(1, Height - 1), Fg, Bg, Box, ColumnWidths, Buffer2).

-doc """
  Renders a complete table widget with headers and data.

  Draws a full table including frame, optional headers, and data rows.

  - `Widget`: Table widget map containing position, dimensions, headers, and rows
  - `Buffer`: Current frame buffer
  - Returns: Updated buffer
""".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    Style = maps:get(style, Widget, double),
    Box = box_styles:Style(),

    ColumnWidths = maps:get(column_widths, Widget, [Width - 1]),

    %% 1. Draw the table frame and grid lines
    Buffer1 = draw_table(X, Y, Height, Fg, Bg, Box, ColumnWidths, Buffer),

    %% 2. Render the header row, if it exists
    Headers = maps:get(headers, Widget, []),
    {HeaderOffset, Buffer2} = case length(Headers) > 0 of
        true ->
            HeaderWidget = #{
                type => widget,
                widget_type => table_row,
                x => X,
                y => Y + 1,
                width => Width,
                height => 1,
                row_data => Headers,
                column_widths => ColumnWidths,
                color => Fg,
                'background-color' => Bg
            },
            {2, widgets:render(HeaderWidget, Buffer1)}; % Header is at Y+1, rows start at Y+2
        false ->
            {1, Buffer1}  % Rows start at Y+1
    end,

    %% 3. Render the data rows
    Rows = maps:get(rows, Widget, []),
    render_rows(X, Y + HeaderOffset, Width, Fg, Bg, ColumnWidths, Rows, Buffer2).


%% @private
-doc "Iterates over the list of rows and renders each one.".
-spec render_rows(integer(), integer(), integer(), atom(), atom(), [non_neg_integer()], [[string()]], map()) -> map().
render_rows(_X, _Y, _Width, _Fg, _Bg, _CW, [], Buffer) -> Buffer;
render_rows(X, Y, Width, Fg, Bg, CW, [Row | Rest], Buffer) ->
    RowWidget = #{
        type => widget,
        widget_type => table_row,
        x => X,
        y => Y,
        width => Width,
        height => 1,
        row_data => Row,
        column_widths => CW,
        color => Fg,
        'background-color' => Bg
    },
    Buffer1 = widgets:render(RowWidget, Buffer),
    render_rows(X, Y + 1, Width, Fg, Bg, CW, Rest, Buffer1).
%%% Editable table implementation to append to table.erl

-doc "Renders the table when it has focus (shows selected cell highlight).".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    % If editing, modify the rows to show edit_text in the cell
    Editing = maps:get(editing, Widget, false),
    WidgetToRender = case Editing of
        true ->
            SelectedRow = maps:get(selected_row, Widget, 0),
            SelectedCol = maps:get(selected_col, Widget, 0),
            EditText = maps:get(edit_text, Widget, ""),
            Rows = maps:get(rows, Widget, []),
            ModifiedRows = update_cell(SelectedRow, SelectedCol, EditText, Rows),
            Widget#{rows => ModifiedRows};
        false ->
            Widget
    end,

    % Render the table with potentially modified rows
    Buffer1 = render(WidgetToRender, Buffer),

    % Add selection highlighting (but skip the edit overlay when editing - rely on modified rows)
    case {maps:get(editable, Widget, false), Editing} of
        {false, _} -> Buffer1;
        {true, true} -> Buffer1;  % When editing, just use the modified rows, no overlay
        {true, false} -> render_selection(WidgetToRender, Buffer1)
    end.

%% @private
-doc "Highlights the currently selected cell.".
-spec render_selection(map(), map()) -> map().
render_selection(Widget, Buffer) ->
    #{x := X, y := Y} = get_common_props(Widget),

    SelectedRow = maps:get(selected_row, Widget, 0),
    SelectedCol = maps:get(selected_col, Widget, 0),
    ColumnWidths = maps:get(column_widths, Widget, []),
    Headers = maps:get(headers, Widget, []),
    Rows = maps:get(rows, Widget, []),
    Editing = maps:get(editing, Widget, false),

    % Validate selection is within bounds
    case SelectedRow >= 0 andalso SelectedRow < length(Rows) andalso
         SelectedCol >= 0 andalso SelectedCol < length(ColumnWidths) of
        false ->
            Buffer;
        true ->
            % Calculate header offset
            HeaderOffset = case length(Headers) > 0 of
                true -> 2;
                false -> 1
            end,

            % Calculate cell position
            CellY = Y + HeaderOffset + SelectedRow,
            {CellX, CellWidth} = calculate_cell_position(X, SelectedCol, ColumnWidths),

            % Render the cell
            case Editing of
                true ->
                    EditText = maps:get(edit_text, Widget, ""),
                    CursorPos = maps:get(edit_cursor_pos, Widget, 0),
                    render_editing_cell(CellX + 1, CellY, CellWidth, EditText, CursorPos, Buffer);
                false ->
                    CellValue = get_cell_value(SelectedRow, SelectedCol, Rows),
                    render_highlighted_cell(CellX + 1, CellY, CellWidth, CellValue, Buffer)
            end
    end.

%%% @private
calculate_cell_position(BaseX, ColIdx, ColumnWidths) ->
    calculate_cell_position(BaseX, ColIdx, ColumnWidths, 0).

calculate_cell_position(BaseX, 0, [Width | _], Offset) ->
    {BaseX + Offset, Width};
calculate_cell_position(BaseX, ColIdx, [Width | Rest], Offset) ->
    calculate_cell_position(BaseX, ColIdx - 1, Rest, Offset + Width + 1);
calculate_cell_position(BaseX, _, [], Offset) ->
    {BaseX + Offset, 10}.

%%% @private
get_cell_value(RowIdx, ColIdx, Rows) when RowIdx >= 0, RowIdx < length(Rows) ->
    Row = lists:nth(RowIdx + 1, Rows),
    case ColIdx >= 0 andalso ColIdx < length(Row) of
        true -> lists:nth(ColIdx + 1, Row);
        false -> ""
    end;
get_cell_value(_, _, _) -> "".

%%% @private
render_highlighted_cell(X, Y, Width, Value, Buffer) ->
    Text = pad_or_truncate(Value, Width),
    cellium_buffer:put_string(X, Y, black, cyan, Text, Buffer).

%%% @private
render_editing_cell(X, Y, Width, Text, CursorPos, Buffer) ->
    DisplayText = pad_or_truncate(Text, Width),
    Buffer1 = cellium_buffer:put_string(X, Y, black, white, DisplayText, Buffer),

    case CursorPos >= 0 andalso CursorPos < Width of
        true ->
            CursorX = X + CursorPos,
            CursorChar = case CursorPos < length(Text) of
                true -> lists:nth(CursorPos + 1, Text);
                false -> $
            end,
            cellium_buffer:set_cell(CursorX, Y, CursorChar, white, black, Buffer1);
        false ->
            Buffer1
    end.

%%% @private
pad_or_truncate(Text, Width) ->
    Len = length(Text),
    if
        Len > Width -> lists:sublist(Text, Width);
        Len < Width -> Text ++ lists:duplicate(Width - Len, $ );
        true -> Text
    end.

%%% @private
extract_state(Widget) ->
    #{
        rows => maps:get(rows, Widget, []),
        selected_row => maps:get(selected_row, Widget, 0),
        selected_col => maps:get(selected_col, Widget, 0),
        editing => maps:get(editing, Widget, false),
        edit_text => maps:get(edit_text, Widget, ""),
        edit_cursor_pos => maps:get(edit_cursor_pos, Widget, 0)
    }.

-doc "Handles keyboard events for editable tables.".
-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, up_key}, Widget) ->
    case maps:get(editing, Widget, false) of
        true -> extract_state(Widget);
        false ->
            SelectedRow = maps:get(selected_row, Widget, 0),
            extract_state(Widget#{selected_row => max(0, SelectedRow - 1)})
    end;

handle_event({key, _, _, _, _, down_key}, Widget) ->
    case maps:get(editing, Widget, false) of
        true -> extract_state(Widget);
        false ->
            SelectedRow = maps:get(selected_row, Widget, 0),
            Rows = maps:get(rows, Widget, []),
            MaxRow = length(Rows) - 1,
            extract_state(Widget#{selected_row => min(MaxRow, SelectedRow + 1)})
    end;

handle_event({key, _, _, _, _, left_key}, Widget) ->
    case maps:get(editing, Widget, false) of
        true ->
            CursorPos = maps:get(edit_cursor_pos, Widget, 0),
            extract_state(Widget#{edit_cursor_pos => max(0, CursorPos - 1)});
        false ->
            SelectedCol = maps:get(selected_col, Widget, 0),
            extract_state(Widget#{selected_col => max(0, SelectedCol - 1)})
    end;

handle_event({key, _, _, _, _, right_key}, Widget) ->
    case maps:get(editing, Widget, false) of
        true ->
            EditText = maps:get(edit_text, Widget, ""),
            CursorPos = maps:get(edit_cursor_pos, Widget, 0),
            extract_state(Widget#{edit_cursor_pos => min(length(EditText), CursorPos + 1)});
        false ->
            SelectedCol = maps:get(selected_col, Widget, 0),
            ColumnWidths = maps:get(column_widths, Widget, []),
            MaxCol = length(ColumnWidths) - 1,
            extract_state(Widget#{selected_col => min(MaxCol, SelectedCol + 1)})
    end;

handle_event({key, _, _, _, _, enter_key}, Widget) ->
    case maps:get(editing, Widget, false) of
        false ->
            SelectedRow = maps:get(selected_row, Widget, 0),
            SelectedCol = maps:get(selected_col, Widget, 0),
            Rows = maps:get(rows, Widget, []),
            CellValue = get_cell_value(SelectedRow, SelectedCol, Rows),
            extract_state(Widget#{editing => true, edit_text => CellValue, edit_cursor_pos => length(CellValue)});
        true ->
            extract_state(finish_editing(Widget))
    end;

handle_event({key, _, _, _, _, escape_key}, Widget) ->
    extract_state(Widget#{editing => false, edit_text => "", edit_cursor_pos => 0});

handle_event({key, _, _, _, _, backspace_key}, Widget) ->
    extract_state(handle_backspace(Widget));

handle_event({key, _, _, _, _, backspace2_key}, Widget) ->
    extract_state(handle_backspace(Widget));

handle_event({key, _, _, _, _, Key}, Widget) when is_binary(Key) ->
    case maps:get(editing, Widget, false) of
        false -> extract_state(Widget);
        true ->
            EditText = maps:get(edit_text, Widget, ""),
            CursorPos = maps:get(edit_cursor_pos, Widget, 0),
            {Before, After} = lists:split(CursorPos, EditText),
            NewText = Before ++ binary_to_list(Key) ++ After,
            extract_state(Widget#{edit_text => NewText, edit_cursor_pos => CursorPos + byte_size(Key)})
    end;

handle_event(_, Widget) ->
    extract_state(Widget).

%%% @private
handle_backspace(Widget) ->
    case maps:get(editing, Widget, false) of
        false -> Widget;
        true ->
            EditText = maps:get(edit_text, Widget, ""),
            CursorPos = maps:get(edit_cursor_pos, Widget, 0),
            case CursorPos of
                0 -> Widget;
                _ when CursorPos > 0 andalso CursorPos =< length(EditText) ->
                    {Before, After} = lists:split(CursorPos, EditText),
                    NewText = lists:sublist(Before, CursorPos - 1) ++ After,
                    Widget#{edit_text => NewText, edit_cursor_pos => CursorPos - 1};
                _ ->
                    Widget
            end
    end.

%%% @private
finish_editing(Widget) ->
    SelectedRow = maps:get(selected_row, Widget, 0),
    SelectedCol = maps:get(selected_col, Widget, 0),
    EditText = maps:get(edit_text, Widget, ""),
    Rows = maps:get(rows, Widget, []),

    UpdatedRows = update_cell(SelectedRow, SelectedCol, EditText, Rows),

    Widget#{
        editing => false,
        edit_text => "",
        edit_cursor_pos => 0,
        rows => UpdatedRows
    }.

%%% @private
update_cell(RowIdx, ColIdx, NewValue, Rows) when RowIdx >= 0, RowIdx < length(Rows) ->
    {BeforeRows, [Row | AfterRows]} = lists:split(RowIdx, Rows),
    UpdatedRow = update_row_cell(ColIdx, NewValue, Row),
    BeforeRows ++ [UpdatedRow | AfterRows];
update_cell(_, _, _, Rows) ->
    Rows.

%%% @private
update_row_cell(ColIdx, NewValue, Row) when ColIdx >= 0, ColIdx < length(Row) ->
    {BeforeCols, [_ | AfterCols]} = lists:split(ColIdx, Row),
    BeforeCols ++ [NewValue | AfterCols];
update_row_cell(_, _, Row) ->
    Row.
