-module(dsl_validation_test).
-include_lib("eunit/include/eunit.hrl").

valid_simple_widget_test() ->
    ?assertEqual(ok, cellium_dsl:validate({button, [{id, b1}], "Click"})).

valid_nested_test() ->
    Dsl = {vbox, [], [
        {header, [], "Title"},
        {hbox, [], [
            {text, [{id, t1}], "Hello"},
            {button, [{id, b1}], "Ok"}
        ]}
    ]},
    ?assertEqual(ok, cellium_dsl:validate(Dsl)).

invalid_tag_test() ->
    {error, {unknown_tag, [not_a_widget]}} = cellium_dsl:validate({not_a_widget, []}),
    {error, {unknown_tag, [invalid, vbox]}} = cellium_dsl:validate({vbox, [], [{invalid, []}]}).

invalid_props_test() ->
    {error, {invalid_props, [button]}} = cellium_dsl:validate({button, not_a_list}),
    {error, {invalid_property, {bad_prop, [text]}}} = cellium_dsl:validate({text, [bad_prop]}).

invalid_children_test() ->
    {error, {expected_children_list, [vbox]}} = cellium_dsl:validate({vbox, [], not_a_list}).

tree_validation_test() ->
    ?assertEqual(ok, cellium_dsl:validate({tree, [], [{"Root", []}]})),
    {error, {expected_nodes_list, [tree]}} = cellium_dsl:validate({tree, [], not_a_list}).

radiogroup_validation_test() ->
    ?assertEqual(ok, cellium_dsl:validate({radiogroup, [], [opt1, opt2]})),
    {error, {expected_options_list, [radiogroup]}} = cellium_dsl:validate({radiogroup, [], not_a_list}).

custom_widget_test() ->
    ?assertEqual(ok, cellium_dsl:validate({custom, my_mod, [{id, c1}]})),
    {error, {invalid_custom, []}} = cellium_dsl:validate({custom, "not_an_atom", []}).
