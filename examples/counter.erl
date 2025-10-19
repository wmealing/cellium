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
    case Msg of
        {tb_event, key, _ ,{keydata, _ ,$+}} ->
            #{count => Count + 1};
        {tb_event, key, _ ,{keydata, _ ,$-}} ->
            #{count => Count - 1};
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            Model
    end.


render(#{count := Count}) ->
    CounterLabel = io_lib:bformat("Counter: ~p (+/-)", [Count]),
    #{type => container,
      id => main_container,
      orientation => horizontal,
      children => [
                   #{type => widget,
                     widget_type => text,
                     id => demo1,
                     value => CounterLabel }]}.

start() ->
   cellium:start(?MODULE).
