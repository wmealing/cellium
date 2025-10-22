-module(boxes).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

simple() ->
    (container:new(container1, vertical))#{id => outer,
                                           expand => true,
                                           children => [

    (container:new(container2, horizontal))#{id => inner1,
                                             expand => true,
                                             children => [
                                                          (box:new(box1, 10, 10))#{ expand => true },
                                                          (box:new(box2, 10, 10))#{ expand => true }
                                                          ]},

     (container:new(container2, horizontal))#{id => inner2,
                                             expand => true,
                                             children => [
                                                          (box:new(box3, 10, 10))#{ expand => true },
                                                          (box:new(box4, 10, 10))#{ expand => true }
                                                          ]}
                                                      
 

                                                       ]}.


start() ->
   cellium:start(#{module => ?MODULE}).

init(_Ignored) ->
    {ok, ""}.

update(Model, Event) ->
    Model.

render(Model) ->
    M = simple(),
    M.

