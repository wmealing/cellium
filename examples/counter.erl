-module(counter).

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    InitialCount = 0,
    Model = #{
        count => InitialCount,
        widget_states => #{
            display => #{text => format_count(InitialCount)}
        }
    },
    {ok, Model}.

update(Model = #{count := Count, widget_states := States}, Msg) ->
    case Msg of
        {button_clicked, plus_btn} ->
            Model#{
                count => Count + 1,
                widget_states => States#{display => #{text => format_count(Count + 1)}}
            };
        {button_clicked, minus_btn} ->
            Model#{
                count => Count - 1,
                widget_states => States#{display => #{text => format_count(Count - 1)}}
            };
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            Model
    end.

render(_Model) ->
    {vbox, [{padding, 1}], [
        {header, [], "Counter Example (Component Pattern)"},
        {spacer, [{size, 1}]},

        % This text widget is automatically populated from widget_states by ID 'display'
        {text, [{id, display}]},

        {spacer, [{size, 1}]},
        {hbox, [{size, 1}], [
            {button, [{id, minus_btn}, {size, 5}], "-"},
            {spacer, [{size, 2}]},
            {button, [{id, plus_btn}, {size, 5}], "+"}
        ]},
        {spacer, [{size, 1}]},
        {text, [], "Press Tab to switch focus, Enter/Space to click, 'q' to quit"}
    ]}.

format_count(Count) ->
    lists:flatten(io_lib:format("Current Count: ~p", [Count])).

start() ->
   application:ensure_all_started(cellium),
   cellium:start(#{module => ?MODULE}).
