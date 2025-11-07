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
-import(widget, [get_common_props/1]).

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
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    Words = maps:get(value, Widget, <<"NO TEXT">>),

    WrappedWords = greedy_wrap:word_wrap(Words, Width),
    logger:info("Wrapping at width: ~p", [Width]),
    draw_lines_of_text(X,Y,Fg,Bg, Height, WrappedWords),
    ok.


draw_line(_X, _Y, _Fg, _Bg, []) ->
    logger:info("EOF"),
    ok;

draw_line(X,Y, Fg, Bg, Word) ->
    logger:info("TEXT: X:~p Y:~p ", [X,Y]),
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

%% Handle an empty line: just recurse, incrementing Y
draw_lines_of_text(X, Y, Fg, Bg, Space, [<<>> | Rest]) ->
    draw_lines_of_text(X, Y + 1, Fg, Bg, Space - 1, Rest);

%% Handle a line with content
draw_lines_of_text(X, Y, Fg, Bg, Space, [FirstLine | Rest]) ->
    draw_line(X, Y, Fg, Bg, FirstLine),
    draw_lines_of_text(X, Y + 1, Fg, Bg, Space - 1, Rest).
%%%===================================================================
%%% Internal functions
%%%===================================================================
