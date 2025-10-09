-module(layout_engine_test).
-include_lib("eunit/include/eunit.hrl").

-export([test_one_child/0]).

-include("cellium.hrl").

layout_engine_test_() ->
    [
        ?_test(test_one_child())
    ].

test_one_child() ->
            LayoutData = #{
                       type => container,
                       id => main_container,
		       x => 0,
		       y => 0,
                       orientation => horizontal,
                       children => [ #{ type => widget,
                                        widget_type => box,
                                        id => box1, 
                                        color => 2,
                                        size => 5 },
                                     #{ type => widget,
                                        widget_type => box,
                                        id => box4,
                                        expand => true
                                      }
                                   ] },
    ScreenWidth = 100,
    ScreenHeight = 20,
    Rendered = layout_engine:calculate_layout(LayoutData, ScreenWidth, ScreenHeight),
%    GoalList = maps_in:get([children], Rendered, default),
%    [GoalElement | _Rest] = GoalList,
%    Goal = maps:get(width ,GoalElement, none),
%    ?assertEqual(100, Goal),
    ok.

test_two_child() ->
            LayoutData = #{
                       type => container,
                       id => main_container,
                       orientation => horizontal,
                       children => [ #{ type => widget,
                                        widget_type => box,
                                        id => input_field,
                                        color => 2,
                                        expand => 10 },
                                     #{ type => widget,
                                        widget_type => box,
                                        id => input_field,
                                        color => 2,
                                        expand => 10 }
                                   ] },
    ScreenWidth = 100,
    Rendered = layout_engine:calculate_layout(LayoutData, ScreenWidth, 20),
    GoalList = maps_in:get([children], Rendered, default),
    [FirstElement, SecondElement] = GoalList,
    FirstGoal = maps:get(width, FirstElement, none),
    SecondGoal = maps:get(width, SecondElement, none ),
    ?assertEqual(50, FirstGoal),
    ?assertEqual(50, SecondGoal),
    ok.

test_one_fixed_child() ->
            LayoutData = #{
                       type => container,
                       id => main_container,
                       orientation => horizontal,
                       children => [ #{ type => widget,
                                        widget_type => box,
                                        id => input_field,
                                        color => 2,
                                        width => 10 },
                                     #{ type => widget,
                                        widget_type => box,
                                        id => input_field,
                                        color => 2,
                                        expand => 10 }
                                   ] },
    ScreenWidth = 100,
    Rendered = layout_engine:calculate_layout(LayoutData, ScreenWidth, 20),
    ?debugFmt("RENDERED: ~p~n", [Rendered]),
    GoalList = maps_in:get([children], Rendered, default),
    [FirstElement, SecondElement] = GoalList,

    FirstGoal = maps:get(width ,FirstElement, none),
    ?assertEqual(10, FirstGoal),

    SecondGoal = maps:get(width, SecondElement, none),
    ?assertEqual(90, SecondGoal),
    ok.



three_should_be_100_total() ->
    LayoutData = #{
                   type => container,
                   id => main_container,
                   orientation => horizontal,
                   children => [ #{ type => widget,
                                    widget_type => box,
                                    id => input_field1,
                                    color => 2,
                                    expand => true },
                                 #{ type => widget,
                                    widget_type => box,
                                    id => input_field2,
                                    color => 2,
                                    expand => true },
                                 #{ type => widget,
                                    widget_type => box,
                                    id => input_field3,
                                    color => 2,
                                    expand => true }
                               ] },
    ScreenWidth = 100,
    Rendered = layout_engine:calculate_layout(LayoutData, ScreenWidth, 20),
    ?debugFmt("RENDERED: ~p~n", [Rendered]),
    GoalList = maps_in:get([children], Rendered, default),
    [FirstElement, SecondElement, ThirdElement] = GoalList,

    FirstGoal  = maps:get(width ,FirstElement,  none),
    SecondGoal = maps:get(width, SecondElement, none),
    ThirdGoal  = maps:get(width, ThirdElement,  none),
    ?assertEqual(100,FirstGoal + SecondGoal + ThirdGoal),
    ok.


