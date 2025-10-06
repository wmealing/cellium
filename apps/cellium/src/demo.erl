-module(demo).

-export([simple/0, position_test/0, nested_layout_data/0, go/0]).

-include_lib("cellium.hrl").

simple_button(Label, Id) ->
    (button:new(Label, foo1))#{id => Id,
                               color => 3,
                               expand => true}.

two_buttons_horizontal() ->
    (container:new(horizontal, container1))#{id => something,
                                             size => 4,
                                             children => [
                                                          simple_button (<<"HI">> , button1),
                                                          simple_button (<<"HELLO">> , button2)]}.

% Example with nested vertical container
nested_layout_data() ->
    #{
        type => container,
        id => main_container,
        x => 0,
        y => 0,
        width => ?TERMBOX:tb_width(),
        height => ?TERMBOX:tb_height(),
        orientation => horizontal,
        children => [
            #{
              type => widget,
              widget_type => box,
              id => fixed_box_left,
              color => 6,
              size => 20
            },
            #{
                type => container,
                id => nested_vertical_container,
                expand => true,
                orientation => vertical,
                children => [
                    #{
                      type => widget,
                      color => 3,
                      widget_type => box,
                      id => middle_box,
                      expand => true
                    },
                    #{
                      type => widget,
                      widget_type => box,
                      id => bottom_box,
                      color => 7,
                      expand => true
                    },
                    two_buttons_horizontal()
                ]
            }
        ]
    }.

simple() ->
    W = nested_layout_data(),
    io:format("NESTED LAYOUT DATA IS: ~p~n", [W]),
    view:start_link(),
    cellium_event_manager:start_link(),
    cellium_state:start_link_local(W).


position_test() ->
    ?TERMBOX:tb_init(),
    ?TERMBOX:tb_clear(),
    ?TERMBOX:tb_set_cell(0,0,$A,0,0),
    ?TERMBOX:tb_set_cell(1,1,$B,0,0),
    ?TERMBOX:tb_present(),
    ok.


go() ->
    X = demo:nested_layout_data(),
    cellium_state:set_model(X).
