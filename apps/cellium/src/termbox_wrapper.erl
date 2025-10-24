-module(termbox_wrapper).

-export([tb_init/0, tb_clear/0, tb_present/0, tb_shutdown/0, tb_set_cell/5]).
-export([tb_width/0, tb_height/0, tb_poll_event/0, tb_print/5]).

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

tb_print(X, Y, Fg, Bg, Str) when is_integer(Bg) and is_integer(Fg) ->
    TermboxBg = ?TB_BLACK,
    TermboxFg = ?TB_WHITE,
    termbox2_nif:tb_print(X, Y, TermboxFg, TermboxBg, Str);

tb_print(X, Y, _Fg, _Bg, Str) ->
    TermboxBg = ?TB_BLACK,
    TermboxFg = ?TB_WHITE,
    termbox2_nif:tb_print(X, Y, TermboxFg, TermboxBg, Str).

tb_width() ->
    termbox2_nif:tb_width().

tb_height() ->
    termbox2_nif:tb_height().

tb_poll_event() ->
    termbox2_nif:tb_poll_event().

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

lookup(What) ->
    1.



