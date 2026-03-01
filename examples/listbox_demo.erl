-module(listbox_demo).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
    cellium:start(#{module => ?MODULE}).

init(_Ignored) ->
    %% Initialize with sample data
    Headers = ["Name", "Age", "Status"],
    Data = [
            ["Adelaide"],
            ["Brisbane" ],
            ["Darwin"],
            ["Hobart"],
            ["Melbourne"],
            ["Perth"],
            ["Sydney"]
    ],

    ColumnWidths = [10],

    TableState = editable_table:start(Headers, Data, ColumnWidths),

    {ok, TableState}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            init:stop(),
            Model;

        %% Up arrow - move selection up
        {key, _, _, _, _, arrow_up} ->
            editable_table:move_cursor(Model, up);

        %% Down arrow - move selection down
        {key, _, _, _, _, arrow_down} ->
            editable_table:move_cursor(Model, down);

        %% Left arrow - move selection left (in edit mode)
        {key, _, _, _, _, arrow_left} ->
            editable_table:move_cursor(Model, left);

        %% Right arrow - move selection right (in edit mode)
        {key, _, _, _, _, arrow_right} ->
            editable_table:move_cursor(Model, right);

        %% Enter key - toggle edit mode
        {key, _, _, _, _, enter_key} ->
            Mode = maps:get(mode, Model),
            case Mode of
                row_select -> editable_table:start_edit(Model);
                cell_edit -> Model
            end;

        %% Escape key - cancel edit mode
        {key, _, _, _, _, escape_key} ->
            editable_table:cancel_edit(Model);

        _Else -> 
            Model
    end.

render(Model) ->
    Headers = maps:get(headers, Model),
    Data = maps:get(data, Model),
    ColumnWidths = maps:get(column_widths, Model),
    CursorRow = maps:get(cursor_row, Model),
    Mode = maps:get(mode, Model),
    CursorCol = maps:get(cursor_col, Model),

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
