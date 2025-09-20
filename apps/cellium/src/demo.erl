-module(demo).

-export([test/0,simple/0]).

-include_lib("cellium.hrl").

%% Black Red Green Yellow Blue Magenta Cyan White ?

simple() ->
        LayoutData = #{
                       type => container,
                       id => main_container,
                       orientation => horizontal,
                       children => [ #{ type => widget,
                                        widget_type => box,
                                        id => input_field,
                                        color => 2,
                                        flex => 10 }
                                   ] },

    Rendered = layout_engine:calculate_layout(LayoutData, 100, 5),
    io:format("RENDERED: ~p~n", [Rendered]),
    init:stop().
    %%   cellium_renderer_server:start_link(),
    %%   cellium_renderer_server:set_root_widget(Rendered).

test() ->
    LayoutData = #{
        type => container,
        id => main_container,
        orientation => vertical,
        children => [
            #{ type => container,
               id => input_container,
               orientation => horizontal,
               children => [ #{ type => widget,
                                widget_type => text_input,
                                id => input_field,
                                flex => 1},
                             #{ type => widget,
                                id => button_ok,
                                widget_type => button,
                                width => 10,
                                height => 3} ] },
            #{ type => container,
               id => content_container,
               orientation => horizontal,
               children => [ #{ type => widget,
                                widget_type => text_area,
                                id => text_area,
                                flex => 1 },
                             #{ type => widget,
                                id => status_bar,
                                widget_type => status_bar,
                                width => 80,
                                height => 1 } ] }
        ]
    },

    RootWidget = layout_engine:calculate_layout(LayoutData, 80, 24),
    io:format("LAYOUT IS: ~p~n", [RootWidget]),
    
    cellium_renderer_server:start_link(),
    cellium_renderer_server:set_root_widget(RootWidget).
