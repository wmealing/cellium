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
                                                          model:maybe_set_focus((box:new(box1, 10, 10))#{ expand => true })
                                                          ]},

     (container:new(container2, horizontal))#{id => inner2,
                                             expand => true,
                                             children => [
                                                          model:maybe_set_focus((box:new(box2, 10, 10))#{ expand => true }),
                                                          model:maybe_set_focus((box:new(box3, 10, 10))#{ expand => true }),
                                                          model:maybe_set_focus((box:new(box4, 10, 10))#{ expand => true })
                                                          ]},
     (container:new(container2, horizontal))#{id => inner3,
                                             expand => true,
                                             children => [
                                                          model:maybe_set_focus((box:new(box5, 10, 10))#{ size => 20 }),
                                                          model:maybe_set_focus((box:new(box6, 10, 10))#{ expand => true })
                                                          ]}
                                                       ]}.


start() ->
   cellium:start(#{module => ?MODULE}).

init(_Ignored) ->
    WidgetList = [box1, box2, box3, box4, box5, box6],

    lists:map(fun(W) -> 
                      focus_manager:register_widget(W)
              end, WidgetList),

    {ok, #{}}.

update(Model, Msg) ->
  case Msg of
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
          cellium:stop(),
          Model;
       Else ->
          Model
  end.

render(_Model) ->
    simple().

