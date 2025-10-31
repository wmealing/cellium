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
                                                          model:maybe_set_focus((box:new(box1, 10, 10))#{ expand => true }),
                                                          model:maybe_set_focus((box:new(box2, 10, 10))#{ expand => true })
                                                          ]},

     (container:new(container2, horizontal))#{id => inner2,
                                             expand => true,
                                             children => [
                                                          model:maybe_set_focus((box:new(box3, 10, 10))#{ expand => true }),
                                                          model:maybe_set_focus((box:new(box4, 10, 10))#{ expand => true })
                                                          ]}
                                                       ]}.


start() ->
   cellium:start(#{module => ?MODULE}).

init(_Ignored) ->
    focus_manager:register_widget(box1),
    focus_manager:register_widget(box2),
    focus_manager:register_widget(box3),
    focus_manager:register_widget(box4),
    {ok, box1}.

update(_Model, _Event) ->
    L = [box1, box2, box3, box4],
    NextFor = lists:zip(L, tl(L) ++ [hd(L)]),
    Next = proplists:get_value(box3, NextFor),
    Next.

render(_Model) ->
    M = simple(),
    M.

