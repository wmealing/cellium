-module(button_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%%% Test suite for button widget
%%% Uses cellium_test_utils for rendering visualization and setup.

button_test_() ->
    {setup,
     fun cellium_test_utils:setup/0,
     fun cellium_test_utils:teardown/1,
     [
      {"Simple Button", fun simple_button_test/0},
      {"Focused Button", fun focused_button_test/0},
      {"Bordered Button (Unfocused)", fun bordered_button_unfocused_test/0},
      {"Bordered Button (Focused)", fun bordered_button_focused_test/0},
      {"Default Label", fun default_label_test/0},
      {"Empty Label", fun empty_label_test/0},
      {"Position Test", fun position_test/0},
      {"Render Focused Delegation", fun render_focused_delegates_to_render_test/0}
     ]}.

%% Tests

simple_button_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(my_button, "Submit"),
    LayoutWidget = Widget#{
        x => 0, y => 0, width => 10, height => 1,
        color => white, 'background-color' => blue,
        focused => false
    },

    ResultBuffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Actual = cellium_test_utils:buffer_to_string(ResultBuffer, 0, 0, 10, 1),
    %% cellium_test_utils:show_render("Simple Button", Actual),
    cellium_test_utils:assert_snapshot("Plain Button H1", " Submit   ", Actual).

focused_button_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(my_button, "OK"),
    LayoutWidget = Widget#{
        x => 5, y => 2, width => 6, height => 1,
        color => white, 'background-color' => blue,
        focused => true
    },

    ResultBuffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Actual = cellium_test_utils:buffer_to_string(ResultBuffer, 5, 2, 4, 1),
    cellium_test_utils:assert_snapshot("Focused Button", "[OK]", Actual).

bordered_button_unfocused_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(my_button, "Click"),
    LayoutWidget = Widget#{
        x => 0, y => 0, width => 12, height => 3,
        color => white, 'background-color' => black,
        focused => false
    },

    ResultBuffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Actual = cellium_test_utils:buffer_to_string(ResultBuffer, 0, 0, 12, 3),
    cellium_test_utils:show_render("Bordered Button (Unfocused)", Actual),
    
    Expected = 
        "┌──────────┐\n"
        "│  Click   │\n"
        "└──────────┘",

    cellium_test_utils:assert_snapshot("Bordered Button  (Unfocused)", Expected, Actual).

bordered_button_focused_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(my_button, "Click"),
    LayoutWidget = Widget#{
        x => 0, y => 0, width => 12, height => 3,
        color => white, 'background-color' => black,
        focused => true
    },

    ResultBuffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Actual = cellium_test_utils:buffer_to_string(ResultBuffer, 0, 0, 12, 3),
    cellium_test_utils:show_render("Bordered Button (Focused)", Actual),
    
    Expected = 
        "╔══════════╗\n"
        "║  Click   ║\n"
        "╚══════════╝",
    ?assertEqual(Expected, Actual).

default_label_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(default_button),
    LayoutWidget = Widget#{
        x => 0, y => 0, width => 10, height => 1,
        color => white, 'background-color' => black,
        focused => false
    },

    ResultBuffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Actual = cellium_test_utils:buffer_to_string(ResultBuffer, 0, 0, 10, 1),
    cellium_test_utils:show_render("Default Label", Actual),
    ?assertEqual(" Button   ", Actual).

empty_label_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(empty_button, ""),
    LayoutWidget = Widget#{
        x => 0, y => 0, width => 4, height => 1,
        color => white, 'background-color' => black,
        focused => false
    },

    ResultBuffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Actual = cellium_test_utils:buffer_to_string(ResultBuffer, 0, 0, 4, 1),
    cellium_test_utils:show_render("Empty Label", Actual),
    ?assertEqual("    ", Actual).

position_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(pos_button, "X"),
    LayoutWidget = Widget#{
        x => 10, y => 5, width => 3, height => 1,
        color => white, 'background-color' => black,
        focused => false
    },

    ResultBuffer = button:render(LayoutWidget, cellium_buffer:empty()),
    Actual = cellium_test_utils:buffer_to_string(ResultBuffer, 10, 5, 3, 1),
    cellium_test_utils:show_render("Position Test", Actual),
    ?assertEqual(" X ", Actual).

render_focused_delegates_to_render_test() ->
    focus_manager:remove_all(),
    terminal_dummy:term_clear(),

    Widget = button:new(test_button, "Test"),
    LayoutWidget = Widget#{
        x => 0, y => 0, width => 8, height => 1,
        color => white, 'background-color' => black,
        focused => true
    },

    Result1 = button:render(LayoutWidget, cellium_buffer:empty()),
    Result2 = button:render_focused(LayoutWidget, cellium_buffer:empty()),

    ?assertEqual(Result1, Result2).
