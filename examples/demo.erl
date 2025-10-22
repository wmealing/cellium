-module(demo).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

% Example with nested vertical container
nested_layout(Model) ->
    #{
        type => container,
        id => main_container,
        class => container_box,
        orientation => horizontal,
        children => [ model:maybe_set_focus(

                       #{type => widget,
                         widget_type => table,
                         class => box,
                         id => table_demo1,
                         size => 20
                        } ),

                      model:maybe_set_focus(

                        #{type => widget,
                          widget_type => table,
                          class => box,
                          id => table_demo2,
                          expand => true } )

                ]
            }.


start() ->
   cellium:start(#{module => ?MODULE,
                   auto_focus => true}).

init(_Ignored) ->
    focus_manager:register_widget(table_demo1),
    focus_manager:register_widget(table_demo2),
    {ok, ""}.

update(Model, _Event) ->
    focus_manager:set_focused(table_demo2),
    Model.

render(Model) ->
    nested_layout(Model).

