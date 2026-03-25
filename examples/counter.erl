%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(counter).

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{count => 1},
    {ok, Model}.

%% this function mutates the model.
update(#{count := Count} = Model, Msg) ->
    logger:info("COUNTER UPDATE MSG: ~p", [Msg]),
    case Msg of
         {key, _, _, _, _, <<"+">>} ->
            Model#{count => Count + 1};
         {key, _, _, _, _, <<"=">>} -> %% Often same key as +
            Model#{count => Count + 1};
         {key, _, _, _, _, <<"-">>} ->
            Model#{count => Count - 1};
         {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
         {key, _, _, _, _, <<"Q">>} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            Model
    end.


render(#{count := Count}) ->
    CounterLabel = io_lib:format("Counter: ~p (+/-)", [Count]),
    {vbox, [{padding, 1}], [
        {text, [{id, demo1}], lists:flatten(CounterLabel)}
    ]}.

start() ->
   application:ensure_all_started(cellium),
   cellium:start(#{module => ?MODULE}).
