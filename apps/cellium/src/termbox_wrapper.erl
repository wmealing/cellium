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

tb_set_cell(X,Y,C,FG,BG) ->
    TermboxFg = lookup(FG),
    TermboxBg = lookup(BG),
    termbox2_nif:tb_set_cell(X,Y,C,TermboxFg,TermboxBg).

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

lookup(black) ->
    ?TB_BLACK;
lookup('red') ->
    ?TB_RED;
lookup('green') ->
    ?TB_GREEN;
lookup('yellow') ->
    ?TB_YELLOW;
lookup('blue') ->
    ?TB_BLUE;
lookup('magenta') ->
    ?TB_MAGENTA;
lookup('cyan') ->
    ?TB_CYAN;
lookup(white) ->
    ?TB_WHITE.





