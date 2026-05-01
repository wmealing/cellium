%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(ex_tree_widget).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include("cellium.hrl").

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE, 
                    color_type => truecolor,
                    auto_focus => true}).

init(_Ignored) ->
    Nodes = [
        {"Project", [
            {"src", [
                {"cellium.erl", []},
                {"tree.erl", []},
                {"widget.erl", []}
            ]},
            {"include", [
                {"cellium.hrl", []}
            ]},
            {"examples", [
                {"ex_tree_widget.erl", []},
                {"list_example.erl", []}
            ]},
            {"rebar.config", []},
            {"README.md", []}
        ]},
        {"Dependencies", [
            {"kernel", []},
            {"stdlib", []}
        ]}
    ],
    {ok, #{
        tree_state => #{
            nodes => Nodes, 
            expanded_ids => ["Project", "src"],
            selected_index => 0, 
            scroll_offset => 0
        }
    }}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        {key, _, _, _, _, _} = KeyEvent ->
            case focus_manager:get_focused() of
                {ok, my_tree} ->
                    NewTreeState = tree:handle_event(KeyEvent, maps:get(tree_state, Model)),
                    Model#{tree_state => NewTreeState};
                _ ->
                    Model
            end;
        _Else ->
            Model
    end.

render(Model) ->
    TreeState = maps:get(tree_state, Model),
    
    {vbox, [{expand, true}], [
        {header, [{color, yellow}], " Tree Widget Example"},
        {vbox, [{padding, 1}, {expand, true}], [
            {frame, [{title, "File Explorer"}, {expand, true}], [
                {tree, [
                    {id, my_tree}, 
                    {nodes, maps:get(nodes, TreeState)},
                    {expanded_ids, maps:get(expanded_ids, TreeState)},
                    {selected_index, maps:get(selected_index, TreeState)},
                    {scroll_offset, maps:get(scroll_offset, TreeState)},
                    {expand, true}
                ]}
            ]}
        ]},
        {status_bar, [], " Arrows: Navigate | Left/Right: Collapse/Expand | 'q' to quit"}
    ]}.
