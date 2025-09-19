-module(demo).

-export([test/0]).

-include_lib("bbox.hrl").

test() ->


   Container = #{
                 direction => row,
                 type => container,
                 size => 10,
                 children => [
                              #{type => textbox, text => "Header", size => 1},
                              #{type => textbox, text => <<"HELLO WORLD">>, flex => 1},
                              #{type => textbox, text => "Footer", size => 1}
                              ]
              },

    RootWidget = #{x => 0, y => 0,
                   size => 30,
                   width => 80,
                   height => 24,
                   type => container,
                   direction =>row, children=>[Container]},

    io:format("LAYOUT IS: ~p~n", [Layout]),
    cellium_renderer_server:start_link(),
    cellium_renderer_server:set_root_widget(RootWidget).

