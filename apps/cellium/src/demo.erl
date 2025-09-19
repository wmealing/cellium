-module(demo).

-export([test/0]).

-include_lib("cellium.hrl").

test() ->
   RootWidget = #{
                 direction => row,
                 type => container,
                 name => "root widget",
                 size => 10,
                 x => 0,
                 y => 0,
                 children => [
                              #{type => box,
                                name => "box box",
                                size => 3,
                                x => 0,
                                y=> 0}
                              ]
              },

    io:format("LAYOUT IS: ~p~n", [RootWidget]),
    cellium_renderer_server:start_link(),
    cellium_renderer_server:set_root_widget(RootWidget).

