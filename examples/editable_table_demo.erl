-module(editable_table_demo).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
    cellium:start(#{module => ?MODULE}).

init(_Ignored) ->
    %% Initialize with sample data
    Headers = ["Name", "Age", "Status"],
    Data = [
        ["Alice",   "28", "Active"],
        ["Bob",     "35", "Active"],
        ["Charlie", "42", "Pending"],
        ["Diana",   "31", "Active"],
        ["Eve",     "29", "Inactive"]
    ],
    ColumnWidths = [20, 8, 15],

    TableState = editable_table:start(Headers, Data, ColumnWidths),

    {ok, TableState}.

update(Model, Msg) ->
    Mode = maps:get(mode, Model),
    case {Mode, Msg} of
        {_, {key, _, _, _, _, <<"q">>}} ->
            init:stop(),
            Model;

        %% Navigation in row selection mode
        {row_select, {key, _, _, _, _, arrow_up}} ->
            editable_table:move_cursor(Model, up);
        {row_select, {key, _, _, _, _, arrow_down}} ->
            editable_table:move_cursor(Model, down);

        %% Navigation in cell edit mode
        {cell_edit, {key, _, _, _, _, arrow_up}} ->
            editable_table:move_cursor(Model, up);
        {cell_edit, {key, _, _, _, _, arrow_down}} ->
            editable_table:move_cursor(Model, down);
        {cell_edit, {key, _, _, _, _, arrow_left}} ->
            editable_table:move_cursor(Model, left);
        {cell_edit, {key, _, _, _, _, arrow_right}} ->
            editable_table:move_cursor(Model, right);
        
        %% Enter key - toggle edit mode or commit edit
        {_, {key, _, _, _, _, enter_key}} ->
            case Mode of
                row_select -> editable_table:start_edit(Model);
                cell_edit -> editable_table:commit_edit(Model)
            end;

        %% Escape key - cancel edit mode
        {cell_edit, {key, _, _, _, _, escape_key}} ->
            editable_table:cancel_edit(Model);

        %% Backspace in cell edit mode
        {cell_edit, {key, _, _, _, _, backspace_key}} ->
            editable_table:delete_char(Model);
        
        %% Character input in cell edit mode
        {cell_edit, {key, _, _, _, _, Char}} when is_binary(Char) ->
            editable_table:handle_char_input(Model, Char);

        _ -> 
            Model
    end.

render(Model) ->
    Headers = maps:get(headers, Model),
    Data = maps:get(data, Model),
    ColumnWidths = maps:get(column_widths, Model),
    CursorRow = maps:get(cursor_row, Model),
    Mode = maps:get(mode, Model),
    CursorCol = maps:get(cursor_col, Model),
    EditBuffer = maps:get(edit_buffer, Model),

    %% Instructions widget
    Instructions = <<"Press UP/DOWN to navigate, ENTER to edit, ESC to cancel, Q to quit">>,

    InstructionsWidget = #{
        id => instructions,
        type => widget,
        widget_type => text,
        value => Instructions,
        size => 1
    },

    %% Create the table widget
    TableWidget = #{
        id => editable_table,
        type => widget,
        widget_type => editable_table,
        width => 50,
        height => 10,
        column_widths => ColumnWidths,
        headers => Headers,
        data => Data,
        rows => Data,
        cursor_row => CursorRow,
        cursor_col => CursorCol,
        mode => Mode,
        edit_buffer => EditBuffer,
        size => 12
    },

    %% Status text
    ModeText = case Mode of
        row_select -> "Mode: Row Selection";
        cell_edit -> io_lib:format("Mode: Editing (Row ~p, Col ~p)", [CursorRow, CursorCol])
    end,

    StatusWidget = #{
        id => status,
        type => widget,
        widget_type => text,
        value => list_to_binary(ModeText), 
        size => 2
    },

    #{
        type => container,
        id => main_container,
        orientation => vertical,
        children => [InstructionsWidget, TableWidget, StatusWidget]
    }.
