%%%-------------------------------------------------------------------
%%% @author Wae Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(mouse_location).

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{x => 0, y => 0 },
    {ok, Model}.

%% this function mutates the model.
update(#{x := X, y := Y} = Model, Msg) ->
    case Msg of
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            cellium:stop(),
            Model;
        {tb_event, mouse, {buttons, _B}, {pos, {MouseX, MouseY}}} ->
            #{x => MouseX, y => MouseY};
        _AnythingElse ->
	    io:format("unknown event: ~p~n", [Msg]), 
            Model
    end.

render(#{x := X, y := Y}) ->
    CounterLabel = io_lib:bformat("Location: ~p:~p ", [X,Y]),
    #{type => container,
      id => main_container,
      size => 10,
      orientation => horizontal,
      children => [
                   #{type => widget,
                     widget_type => text,
	             size => 3,
                     id => demo1,
                     value => CounterLabel }]}.

start() ->
   cellium:start(#{module => ?MODULE, report_mouse => true }).
