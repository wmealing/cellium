-module(ex_stepper).
-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{
        widget_states => #{
            s1 => #{steps => 5, current => 0},
            s2 => #{steps => 3, current => 1, connector => "──"},
            s3 => #{steps => 4, current => 2, connector => " > ", filled_char => "■", empty_char => "□"}
        }
    },
    {ok, Model}.

update(Model, _Msg) ->
    Model.

render(_Model) ->
    {vbox, [{padding, 1}], [
        {header, [], "Stepper Widget Examples"},
        {spacer, [{size, 1}]},
        
        {text, [], "Default Stepper (5 steps, Arrow keys to move):"},
        {stepper, [{id, s1}]},
        {spacer, [{size, 1}]},
        
        {text, [], "Custom Connector (──):"},
        {stepper, [{id, s2}]},
        {spacer, [{size, 1}]},
        
        {text, [], "Custom Characters (■/□) and Connector ( > ):"},
        {stepper, [{id, s3}]},
        
        {spacer, [{expand, true}]},
        {text, [], "Press Tab to switch focus, Left/Right arrows to step, 'q' to quit"}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE}).
