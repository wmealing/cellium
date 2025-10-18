-module(simple_css_test).

-include_lib("eunit/include/eunit.hrl").

-export([test_one/0,test_two/0]).

simple_css_test_() ->
    [
        ?_test(test_one()),
        ?_test(test_two())
    ].

test_one() ->
    Output = css:load_stylesheet("priv/test1.css"),
    Expected = #{{class,primary_button} => #{color => 2, expand => true}},
    Wrong = #{{class,primary_button} => #{}},
    ?assertNotEqual(Output, Wrong),
    ?assertEqual(Output, Expected).

%% we now can see css applied against a layout.
test_two() ->
    Style = css:load_stylesheet("priv/box-test.css"),
    Layout = layout:calculate_layout(test_data:layout_two(),
                                             80,
                                             80),
    StyledLayout = css:style(Layout, Style),
%%    ?debugFmt("STYLED LAYOUT: ~p~n", [StyledLayout]),
    {ok, Widget} = model:find(target_box, StyledLayout),
%%   ?debugFmt("WIDGET: ~p~n", [Widget]),
    #{color := Color } = Widget,
    ?assertEqual (Color , blue),
%%    ?debugFmt("COLOR: ~p~n", [Color]),
    ok.
