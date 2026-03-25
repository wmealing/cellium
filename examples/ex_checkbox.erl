-module(ex_checkbox).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

init(_) -> 
    {ok, #{checked => false}}.

update(Model, {key, _, _, _, _, enter_key}) ->
    Model#{checked => not maps:get(checked, Model)};
update(Model, {key, _, _, _, _, <<"q">>}) ->
    cellium:stop(),
    Model;
update(Model, _) ->
    Model.

render(#{checked := C}) ->
    {vbox, [{padding, 1}], [
        {header, [], "Checkbox Example (Press Enter)"},
        {checkbox, [{id, cb1}, {checked, C}], "Enable Feature"},
        {text, [],
            case C of
                true -> "Feature is ENABLED";
                false -> "Feature is DISABLED"
            end},
        {text, [], "Press 'q' to quit"}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE}).
