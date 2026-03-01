-module(simple_termbox_test).
-export([start/0]).

-include("cellium.hrl").

start() ->
    io:format("Initializing termbox...~n"),
    ?TERMBOX:tb_init(),
    io:format("Clearing termbox...~n"),
    ?TERMBOX:tb_clear(),
    io:format("Setting cell...~n"),
    % X, Y, Char, Fg, Bg
    ?TERMBOX:tb_set_cell(0, 0, $\A, ?TB_WHITE, ?TB_BLACK),
    io:format("Presenting termbox...~n"),
    ?TERMBOX:tb_present(),
    io:format("Press any key to shutdown termbox...~n"),
    ?TERMBOX:tb_poll_event(),
    io:format("Shutting down termbox...~n"),
    ?TERMBOX:tb_shutdown(),
    io:format("Termbox shutdown complete.~n"),
    ok.
