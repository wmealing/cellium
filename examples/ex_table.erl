-module(ex_table).

-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{
        current_style => double,
        show_headers => true,
        active_tab => 0
    },
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        % Toggle table style with 's'
        {key, _, _, _, _, <<"s">>} ->
            CurrentStyle = maps:get(current_style, Model, double),
            NextStyle = case CurrentStyle of
                double -> rounded;
                rounded -> ascii;
                ascii -> double;
                _ -> double
            end,
            Model#{current_style => NextStyle};
        % Toggle headers with 'h'
        {key, _, _, _, _, <<"h">>} ->
            Model#{show_headers => not maps:get(show_headers, Model, true)};
        % Switch tabs with Tab key
        {key, _, _, _, _, tab_key} ->
            Active = maps:get(active_tab, Model, 0),
            Model#{active_tab => (Active + 1) rem 3};
        _ ->
            Model
    end.

render(Model) ->
    CurrentStyle = maps:get(current_style, Model, double),
    ShowHeaders = maps:get(show_headers, Model, true),
    ActiveTab = maps:get(active_tab, Model, 0),

    StyleName = atom_to_list(CurrentStyle),
    HeaderStatus = case ShowHeaders of
        true -> "On";
        false -> "Off"
    end,

    StatusText = io_lib:format(
        "Tab: Switch Examples | S: Style (~s) | H: Headers (~s) | Q: Quit",
        [StyleName, HeaderStatus]
    ),

    % Define headers conditionally
    Headers = case ShowHeaders of
        true -> ["Name", "Age", "City"];
        false -> []
    end,

    ProgrammingHeaders = case ShowHeaders of
        true -> ["Language", "Year", "Paradigm", "VM"];
        false -> []
    end,

    MetricsHeaders = case ShowHeaders of
        true -> ["Metric", "Value", "Change", "Status"];
        false -> []
    end,

    {vbox, [{id, main}, {padding, 0}], [
        {tabs, [{id, table_tabs}, {tabs, ["People", "Languages", "Metrics"]}, {active_tab, ActiveTab}, {expand, true}], [
            % Tab 1: Simple People Table
            {vbox, [{id, tab1}, {expand, true}, {padding, 1}], [
                {header, [{id, h1}, {color, cyan}], "Simple Table Example"},
                {spacer, [{size, 1}]},
                {table, [{id, table1},
                         {style, CurrentStyle},
                         {headers, Headers},
                         {column_widths, [20, 8, 20]},
                         {size, 8},
                         {rows, [
                             ["Alice Johnson", "28", "San Francisco"],
                             ["Bob Smith", "35", "New York"],
                             ["Carol Davis", "42", "Seattle"],
                             ["David Wilson", "31", "Austin"],
                             ["Eve Martinez", "29", "Portland"]
                         ]}]},
                {spacer, [{expand, true}]}
            ]},

            % Tab 2: Programming Languages Table
            {vbox, [{id, tab2}, {expand, true}, {padding, 1}], [
                {header, [{id, h2}, {color, green}], "BEAM Languages"},
                {spacer, [{size, 1}]},
                {table, [{id, table2},
                         {style, CurrentStyle},
                         {headers, ProgrammingHeaders},
                         {column_widths, [15, 8, 15, 12]},
                         {size, 12},
                         {rows, [
                             ["Erlang", "1986", "Functional", "BEAM"],
                             ["Elixir", "2011", "Functional", "BEAM"],
                             ["Gleam", "2019", "Functional", "BEAM"],
                             ["LFE", "2008", "Functional", "BEAM"],
                             ["Alpaca", "2016", "ML-inspired", "BEAM"],
                             ["Hamler", "2020", "Haskell-like", "BEAM"],
                             ["Purerl", "2018", "PureScript", "BEAM"],
                             ["Caramel", "2020", "OCaml", "BEAM"],
                             ["Clojerl", "2015", "Clojure", "BEAM"],
                             ["Efene", "2009", "Python-like", "BEAM"]
                         ]}]},
                {spacer, [{expand, true}]}
            ]},

            % Tab 3: Metrics Dashboard
            {vbox, [{id, tab3}, {expand, true}, {padding, 1}], [
                {header, [{id, h3}, {color, yellow}], "System Metrics"},
                {spacer, [{size, 1}]},
                {table, [{id, table3},
                         {style, CurrentStyle},
                         {headers, MetricsHeaders},
                         {column_widths, [18, 12, 10, 10]},
                         {size, 9},
                         {rows, [
                             ["CPU Usage", "45.2%", "+2.1%", "OK"],
                             ["Memory", "8.4 GB", "+0.3 GB", "OK"],
                             ["Disk I/O", "124 MB/s", "-5 MB/s", "OK"],
                             ["Network In", "15.2 Mb/s", "+3.1 Mb/s", "OK"],
                             ["Network Out", "8.7 Mb/s", "+1.2 Mb/s", "OK"],
                             ["Processes", "1,247", "+15", "OK"],
                             ["Uptime", "45d 12h", "---", "OK"]
                         ]}]},
                {spacer, [{expand, true}]}
            ]}
        ]},
        {status_bar, [{id, sb1}, {color, white}], lists:flatten(StatusText)}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{
        module => ?MODULE,
        auto_focus => false
    }).
