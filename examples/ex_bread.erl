-module(ex_bread).
-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{
        widget_states => #{
            bc1 => #{crumbs => ["Home"]},
            bc2 => #{crumbs => ["Home", "Settings", "Network"]},
            bc3 => #{crumbs => ["Docs", "API", "Widgets", "Breadcrumbs"]},
            bc4 => #{crumbs => ["Root", "usr", "local", "bin"], separator => " / "}
        }
    },
    {ok, Model}.

update(Model, _Msg) ->
    Model.

render(_Model) ->
    {vbox, [{padding, 1}], [
        {header, [], "Breadcrumbs Widget Examples"},
        {spacer, [{size, 1}]},

        {text, [], "Single crumb:"},
        {breadcrumbs, [{id, bc1}, {size, 1}]},
        {spacer, [{size, 1}]},

        {text, [], "Three crumbs (default \" > \" separator):"},
        {breadcrumbs, [{id, bc2}, {size, 1}]},
        {spacer, [{size, 1}]},

        {text, [], "Four crumbs:"},
        {breadcrumbs, [{id, bc3}, {size, 1}]},
        {spacer, [{size, 1}]},

        {text, [], "Custom separator (\" / \"):"},
        {breadcrumbs, [{id, bc4}, {size, 1}]},

        {spacer, [{expand, true}]},
        {text, [], "Press 'q' to quit"}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE}).
