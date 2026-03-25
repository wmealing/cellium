-module(ex_button).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

init(_) -> {ok, #{count => 0}}.
update(Model, {key, _, _, _, _, enter_key}) -> Model#{count => maps:get(count, Model) + 1};
update(Model, {key, _, _, _, _, <<"q">>}) -> cellium:stop(), Model;
update(Model, _) -> Model.
render(#{count := C}) ->
    {vbox, [{padding, 1}], [
        {header, [], "Button Example (Press Enter)"},
        {button, [{id, b1}, {color, green}, {height, 3}], "Click Me!"},
        {button, [{id, b2}, {color, red}, {height, 3}], "Click Me too!"},
        {text, [], lists:flatten(io_lib:format("Clicked ~p times", [C]))},
        {text, [], "Press 'q' to quit"}
    ]}.
start() -> application:ensure_all_started(cellium), cellium:start(#{module => ?MODULE}).
