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
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            cellium:stop(),
            Model;
        {tb_event, key, _ ,{keydata, 0 , 32}} ->
            #{checkbox => not S};
        Else ->
          logger:info("GOT MSG: ~p", [Else]),
          Model
    end.

render(_Model) ->
    W1 = (checkbox:new(one, <<"CHECKBOX1">>))#{checked => true,
                                               color => ?TB_WHITE,
                                               expand => true},
    W2 = (checkbox:new(two, <<"CHECKBOX2">>))#{checked => true,
                                               color => ?TB_WHITE,
                                               expand => true},
    #{type => container,
      id => main_container,
      orientation => vertical,
      children => [ W1, W2 ] }.

