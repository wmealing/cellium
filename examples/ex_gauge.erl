-module(ex_gauge).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

init(_) -> {ok, #{val => 50}}.
update(Model, {key, _, _, _, _, <<"+">>}) -> Model#{val => min(100, maps:get(val, Model) + 5)};
update(Model, {key, _, _, _, _, <<"-">>}) -> Model#{val => max(0, maps:get(val, Model) - 5)};
update(Model, {key, _, _, _, _, <<"q">>}) -> cellium:stop(), Model;
update(Model, _) -> Model.
render(#{val := V}) ->
    {vbox, [{padding, 1}], [
        {header, [], "Gauge Example (Use +/-)"},
        {gauge, [{id, g1}, {value, V}, {label, <<"Power">>}, {width, 40}, {color, yellow}]},
        {text, [], lists:flatten(io_lib:format("Level: ~p%", [V]))},
        {text, [], "Press 'q' to quit"}
    ]}.
start() -> application:ensure_all_started(cellium), cellium:start(#{module => ?MODULE}).
