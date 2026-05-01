-module(gauge_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot Test for Gauge Widget
%% Uses cellium_test_utils for shared logic.

gauge_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Gauge 0%", fun gauge_0/0},
      {"Gauge 50%", fun gauge_50/0},
      {"Gauge 100%", fun gauge_100/0},
      {"Focused Gauge 50%", fun focused_gauge_50/0}
     ]}.

gauge_0() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {gauge, [{id, g0}, {x, 0}, {y, 0}, {width, 10}, {label, <<"Vol">>}, {value, 0}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % Label "Vol" (3) + Gap (1) + Bar (10-3-1 = 6)
    % Value 0 -> 0 filled
    Expected = "Vol ░░░░░░",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:show_render("Gauge 0%", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).

gauge_50() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {gauge, [{id, g50}, {x, 0}, {y, 0}, {width, 10}, {label, <<"Vol">>}, {value, 50}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % 6 * 0.5 = 3 filled
    Expected = "Vol ███░░░",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:show_render("Gauge 50%", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).

gauge_100() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {gauge, [{id, g100}, {x, 0}, {y, 0}, {width, 10}, {label, <<"Vol">>}, {value, 100}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = "Vol ██████",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:show_render("Gauge 100%", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).

focused_gauge_50() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {gauge, [{id, gf}, {x, 0}, {y, 0}, {width, 10}, {label, <<"Vol">>}, {value, 50}, {focusable, true}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    
    focus_manager:set_focus(gf),
    
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = "Vol ███░░░",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:show_render("Gauge 50% Focused", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).
