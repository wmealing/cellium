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
-export([draw_words/7, render/1, new/2]).

-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================
new(Id, Word) ->
    (widget:new())#{id => Id,
                    widget_type => text,
                    value => Word,
                    width => length(Word),
                    height => 1,
                    type => text }.

render(Widget) ->
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),
    Word = maps:get(value, Widget, <<"HELLO WORLD">>),
    draw_word(X,Y,Fg,Bg, Word),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

draw_word(_X, _Y, _Fg, _Bg, []) ->
    ok;

draw_word(X,Y, Bg, Fg, Word) ->
    ?TERMBOX:tb_print(X,
                      Y,
                      Fg,
                      Bg,
                      Word).

draw_words(X1, Y1, _X2, _Y2, Bg, Fg, _Words) ->
    draw_word(X1, Y1, Fg, Bg, <<"HELLO">> ),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
