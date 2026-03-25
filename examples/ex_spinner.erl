-module(ex_spinner).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

init(_) -> erlang:send_after(100, self(), tick), {ok, #{frame => 0}}.
update(Model, tick) -> erlang:send_after(100, self(), tick), Model#{frame => maps:get(frame, Model) + 1};
update(Model, {key, _, _, _, _, <<"q">>}) -> cellium:stop(), Model;
update(Model, _) -> Model.
render(#{frame := F}) ->
    {vbox, [{padding, 1}], [
        {header, [], "Spinner Example"},
        {hbox, [], [
            {text, [], "Loading: "},
            {spinner, [{id, s1}, {frame, F}, {color, cyan}]}
        ]},
        {text, [], "Press 'q' to quit"}
    ]}.
start() -> application:ensure_all_started(cellium), cellium:start(#{module => ?MODULE}).
