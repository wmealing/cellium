-module(demo).

-export([simple/0, position_test/0, nested_model/0, go/0]).

-include_lib("cellium.hrl").

simple_button(Label, Id) ->
    (button:new(Label, foo1))#{id => Id,
                               class => button,
                               expand => true}.

two_buttons_horizontal() ->
    (container:new(container1, horizontal))#{id => something,
                                             size => 4,
                                             children => [
                                                          simple_button ([<<"HI">>] , button1),
                                                          simple_button ([<<"HELLO">>] , button2)]}.

% Example with nested vertical container
nested_model() ->
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
                    }
                ]
            }.
      

simple() ->
    W = nested_model(),
    cellium_event_manager:start_link(),
    app_event_manager:start_link_local(W).

position_test() ->
    ?TERMBOX:tb_init(),
    ?TERMBOX:tb_clear(),
    ?TERMBOX:tb_set_cell(0,0,$A,0,0),
    ?TERMBOX:tb_set_cell(1,1,$B,0,0),
    ?TERMBOX:tb_present(),
    ok.


go() ->
    X = nested_model().

