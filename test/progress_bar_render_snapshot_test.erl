-module(progress_bar_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot Test for Progress Bar Widget
%% Uses cellium_test_utils for shared logic.

progress_bar_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Progress 0%", fun progress_0/0},
      {"Progress 50%", fun progress_50/0},
      {"Progress 100%", fun progress_100/0},
      {"Focused Progress 50%", fun focused_progress_50/0}
     ]}.

progress_0() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {progress_bar, [{id, pb0}, {x, 0}, {y, 0}, {width, 10}, {progress, 0.0}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % width 10 -> [ + 8 chars + ]
    Expected = "[░░░░░░░░]",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:assert_snapshot("Progress 0%", Expected, Actual).

progress_50() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {progress_bar, [{id, pb50}, {x, 0}, {y, 0}, {width, 10}, {progress, 0.5}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % 0.5 * 8 = 4 filled
    Expected = "[████░░░░]",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:assert_snapshot("Progress 50%", Expected, Actual).

progress_100() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {progress_bar, [{id, pb100}, {x, 0}, {y, 0}, {width, 10}, {progress, 1.0}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = "[████████]",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:assert_snapshot("Progress 100%", Expected, Actual).

focused_progress_50() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {progress_bar, [{id, pbf}, {x, 0}, {y, 0}, {width, 10}, {progress, 0.5}, {focusable, true}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    
    focus_manager:set_focus(pbf),
    
    LayoutWidget = layout:calculate_layout(Widget, 10, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % Progress bar render logic swaps Fg/Bg when focused.
    % The characters remain the same.
    Expected = "[████░░░░]",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 1),
    cellium_test_utils:assert_snapshot("Progress 50% Focused", Expected, Actual).
