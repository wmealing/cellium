-module(demo).

-export([nested_layout/0, start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

simple_button(Id, Label) ->
    (button:new(Label, foo1))#{id => Id,
                               class => button,
                               expand => true}.

two_buttons_horizontal() ->
    (container:new(container1, horizontal))#{id => something,
                                             size => 4,
                                             children => [
                                                          simple_button (button1, [<<"HI">>]),
                                                          simple_button (button2, [<<"HELLO">>])]}.

% Example with nested vertical container
nested_layout() ->
    #{
        type => container,
        id => main_container,
        class => container_box,
        width => ?TERMBOX:tb_width(),
        height => ?TERMBOX:tb_height(),
        orientation => horizontal,
        children => [
                    #{
                      type => widget,
                      widget_type => table,
                      class => box,
                      id => table_demo, 
                      expand => true
                    },
		    two_buttons_horizontal()
                ]
            }.



start() ->
   cellium:start(#{module => ?MODULE}).    

init(_Ignored) ->
    {ok, ""}.

update(Model, Event) ->
    Model.

render(Model) ->
    M = nested_layout().

