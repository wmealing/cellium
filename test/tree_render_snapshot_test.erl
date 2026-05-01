-module(tree_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot Test for Tree Widget
%% Uses cellium_test_utils for shared logic.

tree_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Collapsed Tree", fun collapsed_tree/0},
      {"Expanded Tree (Child Insertion)", fun expanded_tree/0},
      {"Nested Expanded Tree", fun nested_expanded_tree/0}
     ]}.

collapsed_tree() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {tree, [{id, t1}, {x, 0}, {y, 0}, {width, 20}, {height, 5}], [
        {"Root A", [
            {"Child A.1", []},
            {"Child A.2", []}
        ]},
        {"Root B", []}
    ]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 20, 5),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % Only roots should be visible
    Expected = 
        "  ▸ Root A          \n"
        "    Root B          \n"
        "                    \n"
        "                    \n"
        "                    ",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 20, 5),
    cellium_test_utils:show_render("Collapsed Tree", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).

expanded_tree() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {tree, [{id, t2}, {x, 0}, {y, 0}, {width, 20}, {height, 5}], [
        {"Root A", [
            {"Child A.1", []},
            {"Child A.2", []}
        ]},
        {"Root B", []}
    ]},
    % Expand "Root A"
    Model = #{widget_states => #{t2 => #{expanded_ids => ["Root A"]}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    LayoutWidget = layout:calculate_layout(Widget, 20, 5),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % Children should be inserted immediately below Root A, preserving Root B's relative order
    Expected = 
        "  ▾ Root A          \n"
        "      Child A.1     \n"
        "      Child A.2     \n"
        "    Root B          \n"
        "                    ",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 20, 5),
    cellium_test_utils:show_render("Expanded Tree", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).

nested_expanded_tree() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {tree, [{id, t3}, {x, 0}, {y, 0}, {width, 20}, {height, 5}], [
        {"Root A", [
            {"Child A.1", [
                {"Grandchild", []}
            ]},
            {"Child A.2", []}
        ]},
        {"Root B", []}
    ]},
    % Expand "Root A" and "Child A.1"
    Model = #{widget_states => #{t3 => #{expanded_ids => ["Root A", "Child A.1"]}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    LayoutWidget = layout:calculate_layout(Widget, 20, 5),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = 
        "  ▾ Root A          \n"
        "    ▾ Child A.1     \n"
        "        Grandchild  \n"
        "      Child A.2     \n"
        "    Root B          ",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 20, 5),
    cellium_test_utils:show_render("Nested Expanded Tree", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).
