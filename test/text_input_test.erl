-module(text_input_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

%% Tests for text_input widget newline behavior

newline_in_single_line_test() ->
    % Initial state: empty text
    State = text_input:new(my_input),
    % height 1, wrap false
    Widget = State#{height => 1, wrap => false},
    
    % Simulate Enter key
    Msg = {key, false, false, false, false, enter_key},
    NewState = text_input:handle_event(Msg, Widget),
    
    % In a single-line input, it should NOT add a newline
    Text = maps:get(text, NewState, ""),
    ?assertEqual("", Text).

newline_in_multi_line_test() ->
    % Initial state: empty text
    State = text_input:new(my_input),
    % height 4, wrap true
    Widget = State#{height => 4, wrap => true},
    
    % Simulate Enter key
    Msg = {key, false, false, false, false, enter_key},
    NewState = text_input:handle_event(Msg, Widget),
    
    % In a multi-line input, it SHOULD add a newline
    Text = maps:get(text, NewState, ""),
    ?assertEqual("\n", Text).

newline_in_default_test() ->
    % Initial state: empty text, no height/wrap specified
    Widget = text_input:new(my_input),
    
    % Simulate Enter key
    Msg = {key, false, false, false, false, enter_key},
    NewState = text_input:handle_event(Msg, Widget),
    
    % By default (no height specified), we should decide if it's single or multi.
    % If it's not specified, maybe it should be single-line by default?
    % Let's see what current behavior is.
    Text = maps:get(text, NewState, ""),
    % Current behavior is that it adds a newline.
    % If we want to change it to single-line by default, this test will fail.
    ?assertEqual("\n", Text).

binary_key_newline_filtering_test() ->
    % Initial state: empty text
    State = text_input:new(my_input),
    % height 1, wrap false
    Widget = State#{height => 1, wrap => false},
    
    % Simulate a binary key containing a newline (e.g. from a paste or raw input)
    Msg = {key, false, false, false, false, <<"line1\nline2">>},
    NewState = text_input:handle_event(Msg, Widget),
    
    % In a single-line input, it should filter out the newline
    Text = maps:get(text, NewState, ""),
    ?assertEqual("line1line2", Text).
