-module(cellium_test_utils).
-moduledoc """
  Utility functions for snapshot testing Cellium widgets.
""".

-export([buffer_to_string/5, assert_snapshot/2, show_render/2, setup/0, teardown/1]).
-include_lib("eunit/include/eunit.hrl").

-doc """
  Converts a rectangular area of a buffer into a newline-separated string.
""".
buffer_to_string(Buffer, StartX, StartY, Width, Height) ->
    Lines = [
        [ begin {C, _, _} = cellium_buffer:get_cell(X, Y, Buffer), lists:flatten([C]) end 
          || X <- lists:seq(StartX, StartX + Width - 1) ]
        || Y <- lists:seq(StartY, StartY + Height - 1)
    ],
    FlattenedLines = [ lists:flatten(L) || L <- Lines ],
    string:join(FlattenedLines, "\n").

-doc """
  Compares an expected ASCII string against the actual buffer output.
  Prints a descriptive error message on failure.
""".
assert_snapshot(Expected, Actual) ->
    case Expected == Actual of
        true -> ok;
        false ->
            io:format(user, "~nSNAPSHOT FAILURE:~n", []),
            io:format(user, "EXPECTED:~n\"~ts\"~n", [unicode:characters_to_binary(Expected)]),
            io:format(user, "ACTUAL:~n\"~ts\"~n", [unicode:characters_to_binary(Actual)]),
            ?assertEqual(Expected, Actual)
    end.

-doc """
  Prints the rendered output to the console with a title.
""".
show_render(Title, Actual) ->
    io:format(user, "~n[ Render: ~s ]~n~ts~n", [Title, unicode:characters_to_binary(Actual)]),
    ok.

-doc "Starts the focus manager and dummy terminal once for a test suite.".
setup() ->
    FocusPid = case whereis(focus_manager) of
        undefined -> 
            {ok, FP} = focus_manager:start_link(),
            FP;
        FP -> FP
    end,
    TermPid = case whereis(terminal_dummy) of
        undefined ->
            {ok, TP} = terminal_dummy:start_link(),
            TP;
        TP -> TP
    end,
    terminal_dummy:term_init(),
    focus_manager:remove_all(),
    {FocusPid, TermPid}.

-doc "Stops the focus manager and dummy terminal.".
teardown(_) ->
    % We don't necessarily want to kill them if other tests are running,
    % but we do want to clear the state.
    focus_manager:remove_all(),
    case whereis(terminal_dummy) of
        undefined -> ok;
        _ -> terminal_dummy:term_clear()
    end,
    ok.
