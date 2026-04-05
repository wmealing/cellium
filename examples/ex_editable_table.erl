-module(ex_editable_table).

-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    % Initial table data
    InitialRows = [
        ["Alice Johnson", "28", "Engineer"],
        ["Bob Smith", "35", "Designer"],
        ["Carol Davis", "42", "Manager"],
        ["David Wilson", "31", "Developer"],
        ["Eve Martinez", "29", "Analyst"]
    ],

    Model = #{
        widget_states => #{
            editable_tbl => #{
                rows => InitialRows,
                selected_row => 0,
                selected_col => 0,
                editing => false,
                edit_text => "",
                edit_cursor_pos => 0
            }
        }
    },
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        _ ->
            Model
    end.

render(Model) ->
    % Get current selection for debug display
    States = maps:get(widget_states, Model, #{}),
    TableState = maps:get(editable_tbl, States, #{}),
    SelectedRow = maps:get(selected_row, TableState, 0),
    SelectedCol = maps:get(selected_col, TableState, 0),
    Editing = maps:get(editing, TableState, false),
    EditText = maps:get(edit_text, TableState, ""),
    CursorPos = maps:get(edit_cursor_pos, TableState, 0),

    % Get current rows to debug
    Rows = maps:get(rows, TableState, []),
    CurrentCellValue = case SelectedRow < length(Rows) of
        true ->
            Row = lists:nth(SelectedRow + 1, Rows),
            case SelectedCol < length(Row) of
                true -> lists:nth(SelectedCol + 1, Row);
                false -> "N/A"
            end;
        false -> "N/A"
    end,

    StatusText = io_lib:format(
        "R:~p C:~p Ed:~p Text:'~s' Cur:~p Cell:'~s'",
        [SelectedRow, SelectedCol, Editing, EditText, CursorPos, CurrentCellValue]
    ),

    {vbox, [{id, main}, {padding, 0}], [
        {header, [{id, h1}, {color, cyan}], "Editable Table Example"},
        {spacer, [{size, 1}]},
        {text, [{id, help}], "Use arrow keys to navigate, Enter to edit cells"},
        {spacer, [{size, 1}]},
        {table, [{id, editable_tbl},
                 {style, double},
                 {editable, true},
                 {focusable, true},
                 {headers, ["Name", "Age", "Role"]},
                 {column_widths, [25, 8, 15]},
                 {size, 8}]},
        {spacer, [{expand, true}]},
        {status_bar, [{id, sb1}, {color, white}], lists:flatten(StatusText)}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{
        module => ?MODULE,
        auto_focus => true
    }).
