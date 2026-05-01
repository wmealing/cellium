-module(stepper_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot Test for Stepper Widget
%% Uses cellium_test_utils for shared logic.

stepper_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Default Stepper (3 steps)", fun default_stepper/0},
      {"Custom Stepper (5 steps, current=2)", fun custom_stepper/0},
      {"Focused Stepper", fun focused_stepper/0}
     ]}.

default_stepper() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {stepper, [{id, s1}, {x, 0}, {y, 0}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 20, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % Default: ( ●━○━○ )
    Expected = "( ●━○━○ )",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 9, 1),
    cellium_test_utils:assert_snapshot("Default stepper", Expected, Actual).

custom_stepper() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {stepper, [{id, s2}, {x, 0}, {y, 0}, {steps, 5}]},
    Model = #{widget_states => #{s2 => #{current => 2}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    LayoutWidget = layout:calculate_layout(Widget, 30, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % 5 steps, current=2: ( ○━○━●━○━○ )
    % Length: "(" (1) + " " (1) + "○" (1) + "━" (1) + "○" (1) + "━" (1) + "●" (1) + "━" (1) + "○" (1) + "━" (1) + "○" (1) + " " (1) + ")" (1) = 13
    Expected = "( ○━○━●━○━○ )",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 13, 1),
    cellium_test_utils:assert_snapshot("Cusom Stepper", Expected, Actual).

focused_stepper() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {stepper, [{id, sf}, {x, 0}, {y, 0}, {steps, 3}, {focusable, true}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    focus_manager:set_focus(sf),
    LayoutWidget = layout:calculate_layout(Widget, 20, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = "( ●━○━○ )",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 9, 1),
    cellium_test_utils:assert_snapshot("Focused Stepper", Expected, Actual).
