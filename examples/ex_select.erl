-module(ex_select).
-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{
        selected_lang => undefined,
        widget_states => #{}
    },
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        {select_changed, sel1, Value} ->
            logger:info("Language selected: ~p", [Value]),
            Model#{selected_lang => Value};
        _ ->
            Model
    end.

render(Model) ->
    Selected = maps:get(selected_lang, Model, "None"),
    {vbox, [{padding, 2}], [
        {header, [{color, cyan}], "Dropdown (Select) Widget Demo"},
        {spacer, [{size, 1}]},
        {text, [], "Choose a language from the dropdown below:"},
        {spacer, [{size, 1}]},
        {select, [{id, sel1}, {options, ["Erlang", "Elixir", "Gleam", "LFE", "Purerl"]}]},
        {spacer, [{size, 2}]},
        {text, [], lists:flatten(io_lib:format("Current selection: ~p", [Selected]))},
        {spacer, [{size, 1}]},
        {text, [{color, yellow}], "Press Enter to open/close, Arrows to navigate, Q to quit."}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{
        module => ?MODULE,
        auto_focus => true
    }).
