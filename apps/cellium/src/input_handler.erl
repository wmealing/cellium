-module(input_handler).
-export([start/1]).

%% @doc Starts the event listener loop.
start(LoopPid) ->
    spawn_link(fun() -> event_loop(LoopPid) end).

event_loop(LoopPid) ->
    case termbox2_nif:poll_event(infinity) of
        {ok, {key, Key}} ->
            gen_server:cast(LoopPid, {user_input, {key, Key}}),
            event_loop(LoopPid);
        {error, _Reason} ->
            ok
    end.

