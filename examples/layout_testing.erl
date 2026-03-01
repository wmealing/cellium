%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(layout_testing).

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

%% this function mutates the model.
update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            Model
    end.

render(_Model) ->
    logger:info("TB HEIGHT IS: ~p", [?TERMBOX:tb_height()]),
    (box:new(box1, 10,10))#{expand => true, id => box1}.

start() ->
   cellium:start(#{module => ?MODULE}).
