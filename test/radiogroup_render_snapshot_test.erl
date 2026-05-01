-module(radiogroup_render_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Snapshot Test for Radiogroup Widget
%% Uses cellium_test_utils for shared logic.

radiogroup_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Vertical Radiogroup (opt_b selected)", fun vertical_radiogroup/0},
      {"Horizontal Radiogroup (opt_b selected)", fun horizontal_radiogroup/0},
      {"Focused Radiogroup (opt_b focused)", fun focused_radiogroup/0}
     ]}.

vertical_radiogroup() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    % Declaration
    Dsl = {radiogroup, [{id, rg_v}, {orientation, vertical}], [opt_a, opt_b, opt_c]},
    % Inject state for selected option
    Model = #{widget_states => #{rg_v => #{selected => opt_b}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    
    % Layout and render
    LayoutWidget = layout:calculate_layout(Widget, 20, 3),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = 
        "( ) opt_a \n"
        "(*) opt_b \n"
        "( ) opt_c ",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 3),
    cellium_test_utils:assert_snapshot("Vertical Radiogroup", Expected, Actual).

horizontal_radiogroup() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {radiogroup, [{id, rg_h}, {orientation, horizontal}], [opt_a, opt_b]},
    Model = #{widget_states => #{rg_h => #{selected => opt_b}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    
    LayoutWidget = layout:calculate_layout(Widget, 30, 1),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % horizontal spacing: 4 + length(Label) + 2
    % opt_a: ( ) opt_a  (11 chars total)
    % opt_b: (*) opt_b  (11 chars total)
    
    Expected = "( ) opt_a  (*) opt_b  ",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 22, 1),
    cellium_test_utils:assert_snapshot("Horizontal Radiogroup", Expected, Actual).

focused_radiogroup() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {radiogroup, [{id, rg_f}, {orientation, vertical}, {focusable, true}], [opt_a, opt_b]},
    % Focus opt_b
    Model = #{widget_states => #{rg_f => #{selected => opt_a, focused_option => opt_b}}},
    Widget = cellium_dsl:from_dsl(Dsl, Model),
    
    % Focus the widget itself in the manager
    focus_manager:set_focus(rg_f),
    
    LayoutWidget = layout:calculate_layout(Widget, 20, 2),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = 
        "(*) opt_a \n"
        "( ) opt_b ",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 10, 2),
    cellium_test_utils:assert_snapshot("Focused Radiogroup", Expected, Actual).
