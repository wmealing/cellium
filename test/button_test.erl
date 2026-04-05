-module(button_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%%% Test suite for button widget
%%%
%%% Note: Requires cellium.hrl to have:
%%% -define(TERMINAL, terminal_dummy).

%% Helper Functions

setup_dummy_terminal() ->
    terminal_dummy:start_link(),
    ?TERMINAL:term_init(),
    ?TERMINAL:term_clear().

teardown_dummy_terminal(_) ->
    ?TERMINAL:term_shutdown(),
    terminal_dummy:stop().

render_widget_to_buffer(Widget) ->
    Buffer = cellium_buffer:empty(),
    button:render(Widget, Buffer).

get_text_at(Buffer, X, Y, Length) ->
    Cells = [cellium_buffer:get_cell(X + I, Y, Buffer) || I <- lists:seq(0, Length - 1)],
    [Char || {Char, _Fg, _Bg} <- Cells].

get_cell_at(Buffer, X, Y) ->
    cellium_buffer:get_cell(X, Y, Buffer).

%% Tests

simple_button_test() ->
    setup_dummy_terminal(),

    Widget = button:new(my_button, "Submit"),
    LayoutWidget = Widget#{
        x => 0,
        y => 0,
        width => 10,
        height => 1,
        color => white,
        'background-color' => blue,
        focused => false
    },

    ResultBuffer = render_widget_to_buffer(LayoutWidget),

    % Unfocused button with height 1 should have spaces around label
    Text = get_text_at(ResultBuffer, 0, 0, 8),
    ?assertEqual(" Submit ", Text),

    % Verify colors are correct (not inverted)
    % Note: colors from widget:get_common_props are converted to integers
    {_Char, Fg, Bg} = get_cell_at(ResultBuffer, 0, 0),
    % We can't easily test exact color values since they're converted to ints
    % Just verify the cell exists and has the expected text
    ?assert(Fg =/= undefined),
    ?assert(Bg =/= undefined),

    teardown_dummy_terminal(ok).

focused_button_test() ->
    setup_dummy_terminal(),

    Widget = button:new(my_button, "OK"),
    LayoutWidget = Widget#{
        x => 5,
        y => 2,
        width => 6,
        height => 1,
        color => white,
        'background-color' => blue,
        focused => true
    },

    ResultBuffer = render_widget_to_buffer(LayoutWidget),

    % Focused button with height 1 should have brackets
    Text = get_text_at(ResultBuffer, 5, 2, 4),
    ?assertEqual("[OK]", Text),

    teardown_dummy_terminal(ok).

bordered_button_unfocused_test() ->
    setup_dummy_terminal(),

    Widget = button:new(my_button, "Click"),
    LayoutWidget = Widget#{
        x => 0,
        y => 0,
        width => 12,
        height => 3,
        color => white,
        'background-color' => black,
        focused => false
    },

    ResultBuffer = render_widget_to_buffer(LayoutWidget),

    % Height >= 3 should render with box border (square style when unfocused)
    % Top-left corner should be square border character (┌)
    {Char, _Fg, _Bg} = get_cell_at(ResultBuffer, 0, 0),
    % BoxStyle = box_styles:square(), TopLeft = "┌"
    ExpectedChar = lists:nth(1, "┌"),
    ?assertEqual(ExpectedChar, Char),

    teardown_dummy_terminal(ok).

bordered_button_focused_test() ->
    setup_dummy_terminal(),

    Widget = button:new(my_button, "Click"),
    LayoutWidget = Widget#{
        x => 0,
        y => 0,
        width => 12,
        height => 3,
        color => white,
        'background-color' => black,
        focused => true
    },

    ResultBuffer = render_widget_to_buffer(LayoutWidget),

    % Height >= 3 with focus should render with double border
    % Top-left corner should be double border character (╔)
    {Char, _Fg, _Bg} = get_cell_at(ResultBuffer, 0, 0),
    % BoxStyle = box_styles:double(), TopLeft = "╔"
    ExpectedChar = lists:nth(1, "╔"),
    ?assertEqual(ExpectedChar, Char),

    teardown_dummy_terminal(ok).

default_label_test() ->
    setup_dummy_terminal(),

    % Test button:new/1 which should have default label "Button"
    Widget = button:new(default_button),
    LayoutWidget = Widget#{
        x => 0,
        y => 0,
        width => 10,
        height => 1,
        color => white,
        'background-color' => black,
        focused => false
    },

    ResultBuffer = render_widget_to_buffer(LayoutWidget),

    % Should contain "Button" as default label
    Text = get_text_at(ResultBuffer, 0, 0, 8),
    ?assertEqual(" Button ", Text),

    teardown_dummy_terminal(ok).

empty_label_test() ->
    setup_dummy_terminal(),

    Widget = button:new(empty_button, ""),
    LayoutWidget = Widget#{
        x => 0,
        y => 0,
        width => 4,
        height => 1,
        color => white,
        'background-color' => black,
        focused => false
    },

    ResultBuffer = render_widget_to_buffer(LayoutWidget),

    % Empty label should just have spaces
    Text = get_text_at(ResultBuffer, 0, 0, 2),
    ?assertEqual("  ", Text),

    teardown_dummy_terminal(ok).

position_test() ->
    setup_dummy_terminal(),

    % Test that button renders at specified position
    Widget = button:new(pos_button, "X"),
    LayoutWidget = Widget#{
        x => 10,
        y => 5,
        width => 3,
        height => 1,
        color => white,
        'background-color' => black,
        focused => false
    },

    ResultBuffer = render_widget_to_buffer(LayoutWidget),

    % Verify text appears at correct position
    Text = get_text_at(ResultBuffer, 10, 5, 3),
    ?assertEqual(" X ", Text),

    % Verify nothing rendered at origin
    {Char, _Fg, _Bg} = get_cell_at(ResultBuffer, 0, 0),
    ?assertEqual($ , Char),  % Should be empty space

    teardown_dummy_terminal(ok).

render_focused_delegates_to_render_test() ->
    setup_dummy_terminal(),

    % button:render_focused/2 should delegate to render/2
    % So rendering with focused flag should give same result
    Widget = button:new(test_button, "Test"),
    LayoutWidget = Widget#{
        x => 0,
        y => 0,
        width => 8,
        height => 1,
        color => white,
        'background-color' => black,
        focused => true
    },

    Buffer = cellium_buffer:empty(),
    Result1 = button:render(LayoutWidget, Buffer),
    Result2 = button:render_focused(LayoutWidget, Buffer),

    % Both should produce identical results
    ?assertEqual(Result1, Result2),

    teardown_dummy_terminal(ok).
