-module(box_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot Test for Box Widget
%% Uses cellium_test_utils for shared logic.

box_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Empty Box (5x3)", fun empty_box/0},
      {"Focused Box (5x3)", fun focused_box/0},
      {"Box with child has focus", fun box_child_focused/0}
     ]}.

empty_box() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),
    Dsl = {box, [{id, box1}, {x, 0}, {y, 0}, {width, 5}, {height, 3}], []},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 5, 3),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = 
        "┌───┐\n"
        "│   │\n"
        "└───┘",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 5, 3),
    cellium_test_utils:assert_snapshot("Empty Box", Expected, Actual).

focused_box() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),
    Dsl = {box, [{id, box2}, {x, 0}, {y, 0}, {width, 5}, {height, 3}, {focusable, true}], []},
    Widget = cellium_dsl:from_dsl(Dsl),
    focus_manager:set_focus(box2),
    LayoutWidget = layout:calculate_layout(Widget, 5, 3),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = 
        "╔═══╗\n"
        "║   ║\n"
        "╚═══╝",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 5, 3),
    cellium_test_utils:assert_snapshot("Focused Box", Expected, Actual).

box_child_focused() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),
    Dsl = {box, [{id, parent_box}, {x, 0}, {y, 0}, {width, 10}, {height, 3}], [
        {button, [{id, child_btn}], "Ok"}
    ]},
    
    Widget = cellium_dsl:from_dsl(Dsl),
    focus_manager:set_focus(child_btn),
    
    LayoutWidget = layout:calculate_layout(Widget, 10, 3),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = 
        "╔════════╗\n"
        "║[Ok]    ║\n"
        "╚════════╝",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 3),
    cellium_test_utils:assert_snapshot("Box Child Focused", Expected, Actual).
