-module(demo).

-export([simple/0]).

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
                    #{
                        type => widget,
                        widget_type => box,
                        id => fixed_box_bottom,
                        size => 3
                    }
                ]
            }
        ]
    }.


simple() ->
    W = tui_layout:realize(tui_layout:nested_layout_data()),
    cellium_renderer_server:start_link(),
    cellium_renderer_server:set_root_widget(W).
%    tui_layout:realize(tui_layout:layout_data()).

% To run this, compile the module: c(tui_layout_realizer).
% Then call: io:format("~p~n",
% Or for the nested example: io:format("~p~n", [tui_layout_realizer:realize(tui_layout_realizer:nested_layout_data())]).



%% position_test() ->
%%     io:format("GO"),
%%     ?TERMBOX:tb_init(),
%%     ?TERMBOX:tb_clear(),
%%     ?TERMBOX:tb_set_cell(0,0,$A, 0,0),
%%     ?TERMBOX:tb_present(),
%%     ok.
