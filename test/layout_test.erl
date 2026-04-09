-module(layout_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

layout_absolute_test() ->
    ?TERMINAL:term_init(),
    Widget = #{
        id => root,
        type => container,
        position => absolute,
        x => 10,
        y => 5,
        width => 20,
        height => 10,
        children => [
            #{id => child1, type => widget, size => 5}
        ]
    },
    Result = layout:calculate_layout(Widget, 100, 40),
    ?assertEqual(10, maps:get(x, Result)),
    ?assertEqual(5, maps:get(y, Result)),
    [Child1] = maps:get(children, Result),
    ?assertEqual(10, maps:get(x, Child1)),
    ?assertEqual(5, maps:get(y, Child1)),
    ?assertEqual(5, maps:get(width, Child1)).

layout_horizontal_test() ->
    ?TERMINAL:term_init(),
    Widget = #{
        id => root,
        type => container,
        orientation => horizontal,
        width => 100,
        height => 10,
        children => [
            #{id => c1, type => widget, size => 10},
            #{id => c2, type => widget, expand => true},
            #{id => c3, type => widget, size => 20}
        ]
    },
    Result = layout:calculate_layout(Widget, 100, 10),
    [C1, C2, C3] = maps:get(children, Result),

    ?assertEqual(0, maps:get(x, C1)),
    ?assertEqual(10, maps:get(width, C1)),

    ?assertEqual(10, maps:get(x, C2)),
    %% 100 - 10 - 20 = 70
    ?assertEqual(70, maps:get(width, C2)),

    ?assertEqual(80, maps:get(x, C3)),
    ?assertEqual(20, maps:get(width, C3)).

layout_vertical_test() ->
    ?TERMINAL:term_init(),
    Widget = #{
        id => root,
        type => container,
        orientation => vertical,
        width => 20,
        height => 50,
        children => [
            #{id => c1, type => widget, size => 10},
            #{id => c2, type => widget, expand => true}
        ]
    },
    Result = layout:calculate_layout(Widget, 20, 50),
    [C1, C2] = maps:get(children, Result),

    ?assertEqual(0, maps:get(y, C1)),
    ?assertEqual(10, maps:get(height, C1)),

    ?assertEqual(10, maps:get(y, C2)),
    %% 50 - 10 = 40
    ?assertEqual(40, maps:get(height, C2)).

layout_padding_test() ->
    ?TERMINAL:term_init(),
    Widget = #{
        id => root,
        type => container,
        padding => #{top => 1, left => 2, right => 3, bottom => 4},
        width => 100,
        height => 20,
        children => [
            #{id => c1, type => widget, expand => true}
        ]
    },
    Result = layout:calculate_layout(Widget, 100, 20),
    [C1] = maps:get(children, Result),

    ?assertEqual(2, maps:get(x, C1)),
    ?assertEqual(1, maps:get(y, C1)),

    %% 100 - 2 - 3 = 95
    ?assertEqual(95, maps:get(width, C1)),

    %% 20 - 1 - 4 = 15
    ?assertEqual(15, maps:get(height, C1)).

layout_centered_test() ->
    ?TERMINAL:term_init(),

    %% terminal_dummy is 100x10
    Widget = #{
        id => root,
        type => container,
        position => centered,
        requested_width => 20,
        requested_height => 4,
        children => []
    },
    Result = layout:calculate_layout(Widget, 100, 10),
    ?assertEqual((100 - 20) div 2, maps:get(x, Result)),
    ?assertEqual((10 - 4) div 2, maps:get(y, Result)),
    ?assertEqual(20, maps:get(width, Result)),
    ?assertEqual(4, maps:get(height, Result)).
