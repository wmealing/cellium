-module(tab_example).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE, 
                    color_type => truecolor,
                    auto_focus => true}).

init(_Ignored) ->
    {ok, #{active_tab => 0}}.

update(Model = #{active_tab := Active}, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        % Shift + Right Arrow to next tab
        {key, true, false, false, false, right_key} ->
            Model#{active_tab => (Active + 1) rem 3};
        % Shift + Left Arrow to previous tab
        {key, true, false, false, false, left_key} ->
            Model#{active_tab => (Active + 2) rem 3};
        _Else ->
            Model
    end.

render(#{active_tab := Active}) ->
    {vbox, [{expand, true}], [
        {tabs, [{id, my_tabs}, {tabs, ["Dashboard", "Logs", "Settings"]}, {active_tab, Active}, {expand, true}], [
            % Dashboard Content
            {vbox, [{padding, 1}, {expand, true}], [
                {header, [], "Dashboard"},
                {text, [], "Welcome to the dashboard. Monitor your system status here."},
                {spacer, [{expand, true}]}
            ]},
            % Logs Content
            {vbox, [{padding, 1}, {expand, true}], [
                {header, [], "System Logs"},
                {text, [], "[INFO] System started"},
                {text, [], "[DEBUG] Loading components..."},
                {text, [], "[INFO] Ready."},
                {spacer, [{expand, true}]}
            ]},
            % Settings Content
            {vbox, [{padding, 1}, {expand, true}], [
                {header, [], "Settings"},
                {text, [], "Configure your application preferences."},
                {hbox, [], [
                    {checkbox, [{id, cb_notify}], "Enable Notifications"}
                ]},
                {hbox, [], [
                    {checkbox, [{id, cb_darkmode}], "Dark Mode"}
                ]},
                {spacer, [{expand, true}]}
            ]}
        ]},
        {status_bar, [], " Use Shift + Left/Right to switch tabs | 'q' to quit"}
    ]}.
