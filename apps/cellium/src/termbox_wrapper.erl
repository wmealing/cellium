-module(termbox_wrapper).

-export([tb_init/0, tb_clear/0, tb_present/0, tb_shutdown/0, tb_set_cell/5]).
-export([tb_width/0, tb_height/0, tb_poll_event/0, tb_print/5]).
-export([tb_set_input_mode/1, tb_set_output_mode/1]).

-include("cellium.hrl").

tb_init() ->
    termbox2_nif:tb_init().

tb_clear() ->
    termbox2_nif:tb_clear().

tb_present() ->
    termbox2_nif:tb_present().

tb_shutdown() ->
    termbox2_nif:tb_shutdown().

tb_set_cell(X,Y,Ch,Bg,Fg) ->
    TermboxBg = lookup(Bg),
    TermboxFg = lookup(Fg),
    termbox2_nif:tb_set_cell(X, Y, Ch, TermboxFg, TermboxBg).

%% tb_print(X, Y, Fg, Bg, Str) when is_integer(Bg) and is_integer(Fg) ->
%%     TermboxBg = lookup(Bg), 
%%     TermboxFg = lookup(Fg),
%%     termbox2_nif:tb_print(X, Y, TermboxFg, TermboxBg, Str);

tb_print(X, Y, Fg, Bg, Str) ->
    TermboxBg = lookup(Bg),
    TermboxFg = lookup(Fg),
    termbox2_nif:tb_print(X, Y, TermboxFg, TermboxBg, Str).

tb_width() ->
    termbox2_nif:tb_width().

tb_height() ->
    termbox2_nif:tb_height().

tb_poll_event() ->
    termbox2_nif:tb_poll_event().

%% #define TB_INPUT_CURRENT    0
%% #define TB_INPUT_ESC        1
%% #define TB_INPUT_ALT        2
%% #define TB_INPUT_MOUSE      4
tb_set_input_mode(Modes) ->
    logger:info("Setting input mode: ~p", [Modes]),
    termbox2_nif:tb_set_input_mode(Modes).

%% #define TB_OUTPUT_CURRENT   0
%% #define TB_OUTPUT_NORMAL    1
%% #define TB_OUTPUT_256       2
%% #define TB_OUTPUT_216       3
%% #define TB_OUTPUT_GRAYSCALE 4
%% #if TB_OPT_ATTR_W >= 32
%% #define TB_OUTPUT_TRUECOLOR 5
%% #endif

tb_set_output_mode(Mode) ->
    logger:info("Setting output mode: ~p", [Mode]),
    termbox2_nif:tb_set_output_mode(Mode).

%% #define TB_DEFAULT              0x0000
%% #define TB_BLACK                0x0001
%% #define TB_RED                  0x0002
%% #define TB_GREEN                0x0003
%% #define TB_YELLOW               0x0004
%% #define TB_BLUE                 0x0005
%% #define TB_MAGENTA              0x0006
%% #define TB_CYAN                 0x0007
%% #define TB_WHITE                0x0008

lookup('white') ->
    ?TB_WHITE;
lookup('cyan') ->
    ?TB_CYAN;
lookup('magenta') ->
    ?TB_MAGENTA;
lookup('blue') ->
    ?TB_BLUE;
lookup('yellow') ->
    ?TB_YELLOW;
lookup('green') ->
    ?TB_GREEN;
lookup('red') ->
    ?TB_RED;
lookup('black') ->
    ?TB_BLACK;
lookup('default') ->
    ?TB_DEFAULT;
lookup('bright_white') ->
    ?TB_BRIGHT bor ?TB_WHITE;
lookup('bright_cyan') ->
    ?TB_BRIGHT bor ?TB_CYAN;
lookup('bright_magenta') ->
    ?TB_BRIGHT bor ?TB_MAGENTA;
lookup('bright_blue') ->
    ?TB_BRIGHT bor ?TB_BLUE;
lookup('bright_yellow') ->
    ?TB_BRIGHT bor ?TB_YELLOW;
lookup(bright_green) ->
    logger:info("BRIGHT GREEN", []),
    ?TB_BRIGHT bor ?TB_GREEN;
lookup('bright_red') ->
    ?TB_BRIGHT bor ?TB_RED;
lookup('bright_black') ->
    ?TB_BRIGHT bor ?TB_BLACK;
lookup(67108872) ->
    ?TB_BRIGHT bor ?TB_RED;
lookup(8) ->
    ?TB_DEFAULT;
lookup(2) ->
    ?TB_DEFAULT;
lookup(1) ->
    ?TB_DEFAULT.






