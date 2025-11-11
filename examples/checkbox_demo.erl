-module(checkbox_demo).
-export([start/0, init/1, update/2, render/1]).

-include("cellium.hrl").

start() ->
    cellium:start(#{module => ?MODULE, color => 256 }).

init(_) ->
    ?TERMBOX:tb_set_output_mode(?TB_OUTPUT_TRUECOLOR),
    {ok, #{checkbox => true }}.

update(#{checkbox :=  S} = Model, Msg) ->
  case Msg of
      {key, _, _, _, _, <<"q">>} ->
          cellium:stop(),
          Model;
      {key,false,false,false,false,<<" ">>}->
          logger:info("GOT MSG: ~p", [S]),
          #{checkbox => not S};
      Else ->
          logger:info("GOT MSG: ~p", [Else]),
          Model
    end.

render(#{checkbox :=  S} = Model) ->

    Checkbox1 = (checkbox:new(one, <<"CHECKBOX 1">>))#{checked => S,
                                                      expand => true},

    #{type => container,
      id => main_container,
      orientation => vertical,
      children => [ Checkbox1] }.

