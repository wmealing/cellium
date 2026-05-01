-module(ex_list).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE, 
                    color_type => truecolor,
                    auto_focus => true}).

init(_Ignored) ->
    Items = [ io_lib:format("Item ~2..0w - This is a scrollable list entry", [I]) || I <- lists:seq(1, 50) ],
    {ok, #{
        list_state => #{items => Items, selected_index => 0, scroll_offset => 0},
        active_tab => 0
    }}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        % Shift + Arrows for tabs
        {key, true, false, false, false, right_key} ->
            Active = maps:get(active_tab, Model, 0),
            Model#{active_tab => (Active + 1) rem 2};
        {key, true, false, false, false, left_key} ->
            Active = maps:get(active_tab, Model, 0),
            Model#{active_tab => (Active + 1) rem 2};
        {key, _, _, _, _, _} = KeyEvent ->
            case focus_manager:get_focused() of
                {ok, my_list} ->
                    NewListState = list:handle_event(KeyEvent, maps:get(list_state, Model)),
                    Model#{list_state => NewListState};
                _ ->
                    Model
            end;
        _Else ->
            Model
    end.

render(Model) ->
    ActiveTab = maps:get(active_tab, Model, 0),
    ListState = maps:get(list_state, Model),
    SelectedIndex = maps:get(selected_index, ListState),
    SelectedItem = lists:nth(SelectedIndex + 1, maps:get(items, ListState)),

    {vbox, [{expand, true}], [
        {tabs, [{id, my_tabs}, {tabs, ["List View", "Details"]}, {active_tab, ActiveTab}, {expand, true}], [
            % Tab 1: Scrollable List
            {vbox, [{padding, 1}, {expand, true}], [
                {header, [], "Select an Item (Use Up/Down Arrows)"},
                {frame, [{title, "Scrollable List"}, {expand, true}], [
                    {list, [{id, my_list}, {items, maps:get(items, ListState)}, 
                            {selected_index, SelectedIndex}, 
                            {scroll_offset, maps:get(scroll_offset, ListState)},
                            {expand, true}]}
                ]}
            ]},
            % Tab 2: Details of selection
            {vbox, [{padding, 1}, {expand, true}], [
                {header, [], "Selection Details"},
                {frame, [{title, "Current Selection"}, {expand, true}], [
                    {text, [], io_lib:format("Index: ~p", [SelectedIndex])},
                    {text, [], io_lib:format("Content: ~s", [SelectedItem])}
                ]}
            ]}
        ]},
        {status_bar, [], " Shift+Arrows: Tabs | Arrows: List | 'q' to quit"}
    ]}.
