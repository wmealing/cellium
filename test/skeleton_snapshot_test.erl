-module(skeleton_snapshot_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% @doc
%% Template for Widget Snapshot Testing.
%% To adapt this for a new widget:
%% 1. Search and replace 'skeleton' with your widget name.
%% 2. Update the DSL in the test cases.
%% 3. Update the 'Expected' ASCII strings.
%% @end

skeleton_snapshot_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,    % Starts focus_manager & terminal_dummy
     fun cellium_test_utils:teardown/1, % Clears state
     [
      {"Basic Rendering", fun basic_render/0},
      {"Focused Rendering", fun focused_render/0}
     ]}.

basic_render() ->
    % 1. Clean state for this specific test
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    % 2. Declaration (DSL)
    % Note: x, y, width, height are required for the layout engine to position it correctly.
    Dsl = {box, [{id, widget1}, {x, 0}, {y, 0}, {width, 5}, {height, 3}], []},
    Widget = cellium_dsl:from_dsl(Dsl),
    
    % 3. Rendering (Layout + Render)
    % Always run layout first to assign absolute coordinates to children and padding.
    LayoutWidget = layout:calculate_layout(Widget, 5, 3),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    % 4. Assertion
    Expected = 
        "┌───┐\n"
        "│   │\n"
        "└───┘",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 5, 3),
    cellium_test_utils:show_render("Skeleton Basic", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).

focused_render() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Dsl = {box, [{id, widget2}, {x, 0}, {y, 0}, {width, 5}, {height, 3}, {focusable, true}], []},
    Widget = cellium_dsl:from_dsl(Dsl),
    
    % Manually set focus via the manager
    focus_manager:set_focus(widget2),
    
    LayoutWidget = layout:calculate_layout(Widget, 5, 3),
    Buffer = widgets:render(LayoutWidget, cellium_buffer:empty()),
    
    Expected = 
        "╔═══╗\n"
        "║   ║\n"
        "╚═══╝",
    Actual = cellium_test_utils:buffer_to_string(Buffer, 0, 0, 5, 3),
    cellium_test_utils:show_render("Skeleton Focused", Actual),
    cellium_test_utils:assert_snapshot(Expected, Actual).
