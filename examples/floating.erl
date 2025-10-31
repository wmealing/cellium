%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%% Floating container demo - displays a floating box at position (5, 5)
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(floating).

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{},
    {ok, Model}.

update(_Model, _Msg) ->
    #{}.

render(_Model) ->
    FloatingBox = floating_container:new(float_box, horizontal, 5, 5, 30, 8),
    FloatingBoxWithChildren =
        FloatingBox#{
        children => [
            #{type => widget,
              widget_type => frame,
              id => content,
              value => <<"This is a floating box!">>,
              x => 2,
              y => 3,
              width => 26,
              height => 5}
        ]
    },
    FloatingBoxWithChildren.

start() ->
    cellium:start(#{module => ?MODULE}).
