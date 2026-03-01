-module(button_demo).

-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{show => demo_one() },
    {ok, Model}.

update(Model, Msg) ->
    logger:info("**************************"),
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        {key,false,false,false,false,<<"1">>} ->
            #{show => demo_one() };
        {key,false,false,false,false,<<"2">>} ->
            #{show => demo_two()};
        Else ->
            logger:info("~p", [Else]),
            Model
    end.

demo_one() ->
    #{type => container,
      id => main_container,
      orientation => vertical,
      children => [
        (button:new(foo1, <<"← HELLO"/utf8>>))#{expand => true},
        (button:new(foo2, <<"HELLO →"/utf8>>))#{expand => true}]
     }.

demo_two() ->
    #{type => container,
      id => main_container,
      orientation => vertical,
      children => [
        (button:new(foo1, <<"DEMO 2"/utf8>>))#{expand => true},
        (button:new(foo2, <<"DEMO 2"/utf8>>))#{expand => true}]
     }.


render(Model) ->
    logger:info(">>>>>>> RENDERING MODEL: ~p~n", [Model]),
    maps:get(show, Model).


start() ->
    cellium:start(#{module => ?MODULE}).

