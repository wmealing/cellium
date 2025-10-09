-module(css_tests).

-include_lib("eunit/include/eunit.hrl").

%% Basic walk_layout tests
walk_layout_simple_widget_test() ->
    Widget = #{id => test1, color => 1},
    Result = css:walk_layout(Widget, fun(W) -> W#{visited => true} end),
    ?assertEqual(true, maps:get(visited, Result)).

walk_layout_nested_test() ->
    Widget = #{
        id => parent,
        children => [
            #{id => child1},
            #{id => child2}
        ]
    },
    Result = css:walk_layout(Widget, fun(W) -> W#{visited => true} end),
    ?assertEqual(true, maps:get(visited, Result)),
    [Child1, Child2] = maps:get(children, Result),
    ?assertEqual(true, maps:get(visited, Child1)),
    ?assertEqual(true, maps:get(visited, Child2)).

%% Stylesheet application tests
apply_class_style_test() ->
    Widget = #{id => test1, class => primary_button},
    Stylesheet = #{
        {class, primary_button} => #{color => 5, expand => true}
    },
    Result = css:style(Widget, Stylesheet),
    ?assertEqual(5, maps:get(color, Result)),
    ?assertEqual(true, maps:get(expand, Result)).

apply_id_style_test() ->
    Widget = #{id => my_widget},
    Stylesheet = #{
        {id, my_widget} => #{color => 7, width => 100}
    },
    Result = css:style(Widget, Stylesheet),
    ?assertEqual(7, maps:get(color, Result)),
    ?assertEqual(100, maps:get(width, Result)).

id_overrides_class_test() ->
    Widget = #{id => my_widget, class => primary_button},
    Stylesheet = #{
        {class, primary_button} => #{color => 5},
        {id, my_widget} => #{color => 9}
    },
    Result = css:style(Widget, Stylesheet),
    ?assertEqual(9, maps:get(color, Result)).

multiple_classes_test() ->
    Widget = #{id => test, class => [btn, large]},
    Stylesheet = #{
        {class, btn} => #{color => 5},
        {class, large} => #{size => 20}
    },
    Result = css:style(Widget, Stylesheet),
    ?assertEqual(5, maps:get(color, Result)),
    ?assertEqual(20, maps:get(size, Result)).

nested_widget_styling_test() ->
    Widget = #{
        id => parent,
        class => container,
        children => [
            #{id => child1, class => button},
            #{id => child2, class => button}
        ]
    },
    Stylesheet = #{
        {class, container} => #{color => 1},
        {class, button} => #{color => 2, expand => true}
    },
    Result = css:style(Widget, Stylesheet),
    ?assertEqual(1, maps:get(color, Result)),
    [Child1, Child2] = maps:get(children, Result),
    ?assertEqual(2, maps:get(color, Child1)),
    ?assertEqual(true, maps:get(expand, Child1)),
    ?assertEqual(2, maps:get(color, Child2)).

%% Parse value tests
parse_integer_value_test() ->
    ?assertEqual(42, css:parse_value("42")),
    ?assertEqual(100, css:parse_value(" 100 ")).

parse_boolean_value_test() ->
    ?assertEqual(true, css:parse_value("true")),
    ?assertEqual(false, css:parse_value("false")).

parse_atom_value_test() ->
    ?assertEqual(horizontal, css:parse_value("horizontal")),
    ?assertEqual(vertical, css:parse_value("vertical")).

%% Parse properties tests
parse_properties_single_test() ->
    Props = css:parse_properties("color: 5"),
    ?assertEqual(#{color => 5}, Props).

parse_properties_multiple_test() ->
    Props = css:parse_properties("color: 5; expand: true; width: 100"),
    ?assertEqual(#{color => 5, expand => true, width => 100}, Props).

%% Parse stylesheet tests
parse_class_selector_test() ->
    CSS = ".button { color: 2; expand: true; }",
    Result = css:parse_stylesheet(CSS),
    Expected = #{{class, button} => #{color => 2, expand => true}},
    ?assertEqual(Expected, Result).

parse_id_selector_test() ->
    CSS = "#main_container { color: 7; width: 100; }",
    Result = css:parse_stylesheet(CSS),
    Expected = #{{id, main_container} => #{color => 7, width => 100}},
    ?assertEqual(Expected, Result).

parse_multiple_selectors_test() ->
    CSS = ".button { color: 2; }\n#my_widget { expand: true; }",
    Result = css:parse_stylesheet(CSS),
    ?assertEqual(2, maps:get(color, maps:get({class, button}, Result))),
    ?assertEqual(true, maps:get(expand, maps:get({id, my_widget}, Result))).
