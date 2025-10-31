-module(table_demo).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
   cellium:start(#{module => ?MODULE}).

init(_Ignored) ->
    {ok, #{}}.

update(Model, Msg) ->

 case Msg of
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            init:stop(),
            Model;
         _Else -> Model
 end.


render(_Model) ->
    logger:debug("DEMOING TABLE"),

    W1 = #{id => table,
           type => widget,
           size => 20,
           column_widths => [20,20,30,10],
           widget_type => table},

    #{type => container,
      id => main_container,
      orientation => vertical,
      children => [ W1 ] }.


