%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(interactive). 

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
#{id => something,
  type => widget,
  expand => true,
  widget_type => frame,
  text => <<"FRAME AS ANYTHING">>,
  padding => #{left => 0,
               right => 0,
               top => 0,
               bottom => 0}}.

start() ->
   cellium:start(#{module => ?MODULE}).
