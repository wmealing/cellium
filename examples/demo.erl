-module(demo).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").



% Example with nested vertical container
nested_layout(_Model) ->
      #{type => container,
        widget_type => frame,
        class => frame,
	width => 20,
	height => 15,
        id => frame1,
        size => 10,
        children => [
                (box:new(foo1, 10, 10))#{ expand => true },
                (box:new(foo2, 10, 10))#{ expand => true }]
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

