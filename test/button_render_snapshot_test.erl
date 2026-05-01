-module(button_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot Test for Button Widget
%% Uses cellium_test_utils for shared logic.

button_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Plain Button (Height 1)", fun plain_button_h1/0},
      {"Focused Button (Height 1)", fun focused_button_h1/0},
      {"Plain Button (Height 3)", fun plain_button_h3/0},
      {"Focused Button (Height 3)", fun focused_button_h3/0}
     ]}.

plain_button_h1() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),
    Dsl = {button, [{id, btn1}, {x, 0}, {y, 0}, {width, 10}, {height, 1}], "Click Me"},
    Widget = cellium_dsl:from_dsl(Dsl),
    Buffer = button:render(Widget, cellium_buffer:empty()),
    Expected = " Click Me ",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:assert_snapshot("Plain Button H1", Expected, Actual).

focused_button_h1() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),
    Dsl = {button, [{id, btn2}, {x, 0}, {y, 0}, {width, 10}, {height, 1}, {focusable, true}], "Click Me"},
    Widget = cellium_dsl:from_dsl(Dsl),
    focus_manager:set_focus(btn2),
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Expected = "[Click Me]",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:assert_snapshot("Focused Button H1", Expected, Actual).

plain_button_h3() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),
    Dsl = {button, [{id, btn3}, {x, 0}, {y, 0}, {width, 12}, {height, 3}], "Submit"},
    Widget = cellium_dsl:from_dsl(Dsl),
    Buffer = button:render(Widget, cellium_buffer:empty()),
    Expected = 
        "┌──────────┐\n"
        "│  Submit  │\n"
        "└──────────┘",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 12, 3),
    cellium_test_utils:assert_snapshot("Plain Button H3", Expected, Actual).

focused_button_h3() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),
    Dsl = {button, [{id, btn4}, {x, 0}, {y, 0}, {width, 12}, {height, 3}, {focusable, true}], "Submit"},
    Widget = cellium_dsl:from_dsl(Dsl),
    focus_manager:set_focus(btn4),
    LayoutWidget = layout:calculate_layout(Widget, 12, 3),
    Buffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Expected = 
        "╔══════════╗\n"
        "║  Submit  ║\n"
        "╚══════════╝",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 12, 3),
    cellium_test_utils:assert_snapshot("Focused Button H3", Expected, Actual).
