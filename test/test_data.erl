-module(test_data).

-export([layout_one/0, layout_two/0]).

-include("cellium.hrl").

layout_two() ->
    #{ type => container,
       id => main_container,
       class => container_box,
       x => 0,
       y => 0,
       orientation => horizontal,
       children => [
                    #{
                      type => widget,
                      widget_type => box,
                      id => target_box,
                      class => box,
                      size => 20 } ] }.

layout_one() ->
    #{
        type => container,
        id => main_container,
        class => container_box,
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
              color => red,
              class => box,
              size => 20
            } ]
            }.
