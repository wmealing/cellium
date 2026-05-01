-module(breadcrumbs_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot tests for the Breadcrumbs widget.
%% Uses cellium_test_utils for shared setup/teardown logic.

breadcrumbs_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Empty crumbs",          fun empty_crumbs/0},
      {"Single crumb",          fun single_crumb/0},
      {"Three crumbs",          fun three_crumbs/0},
      {"Custom separator",      fun custom_separator/0},
      {"Focused breadcrumbs",   fun focused_crumbs/0}
     ]}.

empty_crumbs() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {breadcrumbs, [{id, bc_empty}, {x, 0}, {y, 0}, {width, 20}]},
    Widget = cellium_dsl:from_dsl(Dsl),
    LayoutWidget = layout:calculate_layout(Widget, 20, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),

    Expected = "",
    Actual   = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 0, 1),
    cellium_test_utils:assert_snapshot("Empty crumbs", Expected, Actual).

single_crumb() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {breadcrumbs, [{id, bc1}, {x, 0}, {y, 0}, {width, 20}]},
    Model = #{widget_states => #{bc1 => #{crumbs => ["Home"]}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    LayoutWidget = layout:calculate_layout(Widget, 20, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),

    Expected = "Home",
    Actual   = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 4, 1),
    cellium_test_utils:assert_snapshot("Single crumb", Expected, Actual).

three_crumbs() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {breadcrumbs, [{id, bc2}, {x, 0}, {y, 0}, {width, 30}]},
    Model = #{widget_states => #{bc2 => #{crumbs => ["Home", "Settings", "Network"]}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    LayoutWidget = layout:calculate_layout(Widget, 30, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),

    %% "Home > Settings > Network" = 25 chars
    Expected = "Home > Settings > Network",
    Actual   = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 25, 1),
    cellium_test_utils:assert_snapshot("Three crumbs", Expected, Actual).

custom_separator() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {breadcrumbs, [{id, bc3}, {x, 0}, {y, 0}, {width, 20}]},
    Model = #{widget_states => #{bc3 => #{crumbs => ["A", "B", "C"], separator => " / "}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    LayoutWidget = layout:calculate_layout(Widget, 20, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),

    %% "A / B / C" = 9 chars
    Expected = "A / B / C",
    Actual   = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 9, 1),
    cellium_test_utils:assert_snapshot("Custom separator", Expected, Actual).

focused_crumbs() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {breadcrumbs, [{id, bc_f}, {x, 0}, {y, 0}, {width, 20}, {focusable, true}]},
    Model = #{widget_states => #{bc_f => #{crumbs => ["Home", "Profile"]}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    focus_manager:set_focus(bc_f),
    LayoutWidget = layout:calculate_layout(Widget, 20, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),

    %% "Home > Profile" = 14 chars; colours are inverted but characters are the same
    Expected = "Home > Profile",
    Actual   = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 14, 1),
    cellium_test_utils:assert_snapshot("Focused breadcrumbs", Expected, Actual).
