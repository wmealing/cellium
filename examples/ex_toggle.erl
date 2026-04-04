-module(ex_toggle).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

init(_) -> {ok, #{}}.

update(Model, {key, _, _, _, _, <<"q">>}) -> 
    cellium:stop(), 
    Model;
update(Model, _) -> 
    Model.

render(_) ->
    {vbox, [{padding, 1}], [
        {header, [{color, green}], "Toggle Example (Press Space/Enter to Toggle)"},

        {spacer, [{size, 1}]},

        {hbox, [], [
            {text, [{size, 18}], "Standard Toggle: "},
            {toggle, [{id, tg1}, {size, 8}]}
        ]},

        {spacer, [{size, 1}]},

        {hbox, [], [
            {text, [{size, 18}], "Another Toggle: "},
            {toggle, [{id, tg2}, {size, 8}]}
        ]},

        {spacer, [{size, 2}]},

        {text, [], "Press Tab to switch between toggles."},
        {text, [], "Press 'q' to quit"}
    ]}.

start() -> 
    application:ensure_all_started(cellium), 
    cellium:start(#{module => ?MODULE}).
