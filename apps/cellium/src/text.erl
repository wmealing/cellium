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
-export([draw_lines_of_text/6, render/1, new/2]).

-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================
new(Id, Words) ->
    (widget:new())#{id => Id,
                    type => widget,
                    widget_type => text,
                    value => Words,
                    width => bit_size(Words),
                    height => 1
}.

render(Widget) ->
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    Words = maps:get(value, Widget, <<"NO TEXT">>),

    WrappedWords = greedy_wrap:word_wrap(Words, Width),
    draw_lines_of_text(X,Y,Fg,Bg, Height - 1, WrappedWords),
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

%% terminate early if there is no 'lines' left.
draw_lines_of_text(_X, _Y,  _Bg, _Fg, 0, _l) ->
    ok;

%% terminate early if there is no content left.
draw_lines_of_text(_X, _Y,  _Bg, _Fg, _Space, []) ->
    ok;


%% otherwise draw the lines.
draw_lines_of_text(X, Y, Bg, Fg, Space, WordList) ->
    [FirstLine | Rest] = WordList,
    case FirstLine of
        <<"">> ->
            draw_word(X, Y, Fg, Bg, <<"\n">>);
        _ ->
            draw_word(X, Y, Fg, Bg, FirstLine)
    end,

    draw_lines_of_text(X, Y + 1, Fg, Bg, Space - 1 , Rest).

%%%===================================================================
%%% Internal functions
%%%===================================================================
