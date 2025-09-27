-module(demo).

-export([simple/0, position_test/0, go/0]).

-include_lib("cellium.hrl").

%% Black Red Green Yellow Blue Magenta Cyan White ?


% Example provided data (adapted for Erlang maps)
layout_data() ->
    #{
        type => container,
        id => main_container,
        x => 0,
        y => 0,
        width => 80,
        height => 24,
        orientation => horizontal,
        children => [
            #{
                type => widget,
                widget_type => box,
                id => box1,
                color => 2,
                size => 5
            },
            #{
                type => widget,
                widget_type => box,
                id => box4,
                expand => true
            }
        ]
    }.

% Example with nested vertical container
nested_layout_data() ->
    #{
        type => container,
        id => main_container,
        x => 0,
        y => 0,
        width => 80,
        height => 24,
        orientation => horizontal,
        children => [
            #{
                type => widget,
                widget_type => box,
                id => fixed_box_left,
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
                        widget_type => box,
                        id => top_box,
                        size => 5
                    },
                    #{
                        type => widget,
                        widget_type => box,
                        id => middle_box,
                        expand => true
                    },
                    #{
                        type => widget,
                        widget_type => box,
                        id => bottom_box,
                        expand => true
                    },
                    (container:new(horizontal, container1))#{id => something,
                                                             x => 0,
                                                             y => 0,
                                                             width => 80,
                                                             height => 24,
                                                             children => [
                                                                          (button:new(<<"HELLO">>, foo1))#{id => button1, expand => true},
                                                                          (button:new(<<"THERE">>, foo2))#{id => button2, expand => true}]}
                ]
            }
        ]
    }.

broken_layout_data() ->
    (container:new(horizontal, container1))#{
                                             id => something,
                                             x => 0,
                                             y => 0,
                                             width => 80,
                                             height => 10,
                                             children => [
                        (button:new(<<"HELLO">>, foo1))#{id => box1,
                                                         size => 20
                                                        },
                        (button:new(<<"THERE">>, foo2))#{id => box2,
                                                         expand => true
                                                        }
                                                         ]}.


simple() ->
    W = layout_engine:calculate_layout(nested_layout_data()),
    cellium_renderer_server:start_link(),
    cellium_renderer_server:set_root_widget(W).

go() ->
    io:format("Demo function is running!~n", []),
    cellium_event_manager:start_link(),
    % Force stdout to be sent immediately before stopping
    timer:sleep(3000),
    init:stop().


position_test() ->
    io:format("GO"),
    ?TERMBOX:tb_init(),
    ?TERMBOX:tb_clear(),
    ?TERMBOX:tb_set_cell(0,0,$A,0,0),
    ?TERMBOX:tb_set_cell(1,1,$B,0,0),
    ?TERMBOX:tb_present(),
    ok.
