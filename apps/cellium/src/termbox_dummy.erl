-module(termbox_dummy).

-export([tb_init/0, tb_clear/0, tb_present/0, tb_shutdown/0, tb_set_cell/5]).
-export([tb_width/0, tb_height/0, tb_poll_event/0]).

tb_init() ->
    ok.

tb_clear() ->
    ok.

tb_present() ->
    ok.

tb_shutdown() ->
    ok.

tb_set_cell(_X,_Y, _C, _FG, _BG) ->
	ok. 

tb_width() ->
	80.

tb_height() ->
	24.

tb_poll_event() ->
    ok.
