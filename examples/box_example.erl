-module(box_example).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
    application:ensure_all_started(cellium),
    logger:debug("BOX_EXAMPLE:START()"),
    cellium:start(#{module => ?MODULE, 
                    color_type => truecolor,
                    auto_focus => true}).

init(_Ignored) ->
    logger:debug("BOX_EXAMPLE:INIT"),
    {ok, #{}}.

update(Model, Msg) ->
    logger:debug("BOX_EXAMPLE:UPDATE"),
    case Msg of
        {key,_ ,_ ,_ ,_,<<"q">>} ->
            cellium:stop(),
            Model;
        _Else ->
            Model
    end.

render(_Model) ->
    logger:debug("BOX_EXAMPLE:RENDER"),
    {box, [{expand, true},{id, foo}], []}.

