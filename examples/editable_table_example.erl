-module(editable_table_example).
-export([demo/0]).

-include("cellium.hrl").

demo() ->
    %% Sample data
    Headers = ["Name", "Age", "Status"],
    Data = [
        ["Alice", "28", "Active"],
        ["Bob", "35", "Active"],
        ["Charlie", "42", "Pending"],
        ["Diana", "31", "Active"]
    ],
    ColumnWidths = [15, 10, 12],
    
    %% Create the editable table state
    TableState = editable_table:start(Headers, Data, ColumnWidths),
    
    %% Create a widget for rendering
    Widget = maps:merge(TableState, #{
        x => 2,
        y => 2,
        width => 40,
        height => 8,
        style => double,
        fg => ?TB_WHITE,
        bg => ?TB_BLACK
    }),
    
    %% Initial render
    editable_table:render(Widget),
    
    %% Example navigation: move down to row 1
    TableState2 = editable_table:move_cursor(TableState, down),
    Widget2 = maps:merge(TableState2, #{
        x => 2,
        y => 2,
        width => 40,
        height => 8,
        style => double,
        fg => ?TB_WHITE,
        bg => ?TB_BLACK
    }),
    
    %% Clear and re-render
    ?TERMBOX:tb_clear(),
    editable_table:render(Widget2),
    ?TERMBOX:tb_present(),
    
    %% Example: enter edit mode
    TableState3 = editable_table:start_edit(TableState2),
    Widget3 = maps:merge(TableState3, #{
        x => 2,
        y => 2,
        width => 40,
        height => 8,
        style => double,
        fg => ?TB_WHITE,
        bg => ?TB_BLACK
    }),
    
    ?TERMBOX:tb_clear(),
    editable_table:render(Widget3),
    ?TERMBOX:tb_present(),
    
    %% Example: commit an edit
    TableState4 = editable_table:commit_edit(TableState3, "Robert"),
    
    io:format("Updated data: ~p~n", [maps:get(data, TableState4)]),
    
    ok.
