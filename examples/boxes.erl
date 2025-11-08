-module(boxes).



-import(model, [maybe_set_focus/1]).
-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
   cellium:start(#{module => ?MODULE, color_type => truecolor}).


simple() ->
    (container:new(container1, vertical))#{id => outer,
                                           expand => true,
                                           children => [

    (container:new(container2, horizontal))#{id => inner1,
                                             expand => true,
                                             children => [
                                                          maybe_set_focus((box:new(box1, 10, 10))#{ expand => true,
												   color => "000000", 
                                                                                                   'background-color' => "FFFFFF"
                                                                                                  })
                                                          ]},

     (container:new(container2, horizontal))#{id => inner2,
                                             expand => true,
                                             children => [
                                                          maybe_set_focus((box:new(box2, 10, 10))#{ expand => true,
                                                                                                   'background-color' => "F77FBE"}),
                                                          maybe_set_focus((box:new(box3, 10, 10))#{ expand => true,
                                                                                                   'background-color' => "EADFD0"}),
                                                          maybe_set_focus((box:new(box4, 10, 10))#{ expand => true,
                                                                                                    'background-color' => "EEEE77"})
                                                          ]},
     (container:new(container2, horizontal))#{id => inner3,
                                             expand => true,
                                             children => [
                                                          maybe_set_focus((box:new(box5, 10, 10))#{ size => 20,
                                                                                                    'background-color' => "6ADC99"}),
                                                          maybe_set_focus((box:new(box6, 10, 10))#{ expand => true,
                                                                                                    color => "FFFFFF",
                                                                                                    'background-color' => "1C1B1A"})
                                                          ]}
                                                       ]}.



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
       _Else ->
          Model
  end.

render(_Model) ->
    S = simple(),
    logger:info("WIDGET: ~p", [S]),
    S.



