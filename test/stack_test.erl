-module(stack_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

stack_dsl_test() ->
    Dsl = {stack, [{id, my_stack}], [
        {text, [{id, t1}], "layer 1"},
        {text, [{id, t2}], "layer 2"}
    ]},
    Widget = cellium_dsl:from_dsl(Dsl),
    ?assertEqual(stack, maps:get(widget_type, Widget)),
    ?assertEqual(container, maps:get(type, Widget)),
    Children = maps:get(children, Widget),
    ?assertEqual(2, length(Children)),
    [C1, C2] = Children,
    ?assertEqual(text, maps:get(widget_type, C1)),
    ?assertEqual(text, maps:get(widget_type, C2)).

stack_layout_test() ->
    ?TERMINAL:term_init(),
    % Use width/height and also x/y to ensure we can assert on them
    Dsl = {stack, [{id, my_stack}, {x, 0}, {y, 0}, {width, 20}, {height, 5}], [
        {text, [{id, t1}, {expand, true}], "layer 1"},
        {text, [{id, t2}, {expand, true}], "layer 2"}
    ]},
    Widget = cellium_dsl:from_dsl(Dsl),
    Layout = layout:calculate_layout(Widget, 100, 40),
    
    ?assertEqual(0, maps:get(x, Layout)),
    ?assertEqual(0, maps:get(y, Layout)),
    % Note: calculate_layout/3 overrides width/height of the root widget with passed values
    ?assertEqual(100, maps:get(width, Layout)),
    ?assertEqual(40, maps:get(height, Layout)),
    
    [C1, C2] = maps:get(children, Layout),
    
    % Both children should have the same coordinates (0,0) and size as the stack
    ?assertEqual(0, maps:get(x, C1)),
    ?assertEqual(0, maps:get(y, C1)),
    ?assertEqual(100, maps:get(width, C1)),
    ?assertEqual(40, maps:get(height, C1)),
    
stack_render_test() ->
    ?TERMINAL:term_init(),
    Dsl = {stack, [{id, my_stack}, {x, 0}, {y, 0}, {width, 10}, {height, 1}], [
        {text, [{id, t1}], "layer 1"},
        {text, [{id, t2}], "layer 2"}
    ]},
    Widget = cellium_dsl:from_dsl(Dsl),
    Layout = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(Layout),
    
    % Since they are rendered in order, t2 should overwrite t1 if they occupy the same space.
    % In this case, they both start at (0,0).
    % "layer 2" should be in the buffer.
    
    % Let's check a few characters
    {C1, _, _, _, _} = cellium_buffer:get_cell(0, 0, Buffer),
    ?assertEqual($l, C1),
    {C7, _, _, _, _} = cellium_buffer:get_cell(6, 0, Buffer),
    ?assertEqual($2, C7).
