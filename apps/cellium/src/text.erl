%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 26 Sep 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(text).

%% API
-export([draw_words/5, draw/5]).

-include_lib("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%  1   2   3   4   5
draw(X1, Y1, X2, Y2, Words) ->
    draw_words(X1, Y1, X2, Y2, [Words]).

draw_words(_, Y, _, Y, []) ->
    ok;

draw_words(_, _, _, _, []) ->
    ok;

draw_words(X1, Y1, X2, Y2, Words) ->

    #{bg := Bg, fg := Fg} = theme:load(text),

    [FirstWord | Rest] =  Words,

    ?TERMBOX:tb_print(X1 +1,
                      Y1 +1,
                      Fg,
                      Bg,
                      FirstWord),

    draw_words(X1,
               Y1 + 1,
               X2,
               Y2, Rest),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
