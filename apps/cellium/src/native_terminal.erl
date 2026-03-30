-module(native_terminal).
-moduledoc """
  A module for interacting with the native terminal, providing functions for
  initializing, shutting down, drawing, and handling input events.
  It aims to abstract away terminal-specific escape codes and provide a
   consistent interface for terminal-based applications.
""".

-export([
    tb_init/0,
    tb_shutdown/0,
    tb_width/0,
    tb_height/0,
    tb_clear/0,
    tb_present/0,
    tb_set_cell/5,
    tb_print/5,
    get_cell/2,
    tb_get_row/2,
    tb_get_row/3,
    tb_poll_event/0,
    get_next_event/0,
    tb_set_input_mode/1,
    tb_set_output_mode/1,
    tb_force_redraw/0
]).

%% Internal state management
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-record(state, {
    width :: integer(),
    height :: integer(),
    input_mode :: default | alt,
    output_mode :: default | 2 | 256 | truecolor,
    char_buffer = [] :: list(char()),
    event_buffer = [] :: list(term()),
    buffer_time = undefined :: undefined | integer(),
    waiting_client = undefined :: undefined | pid(),
    back_buffer = #{} :: map(),
    front_buffer = #{} :: map()
}).


%% Character constants
-define(ESC, 27).
-define(CSI_START, 91).  % '['
-define(SS3_START, 79).  % 'O'
-define(TILDE, 126).     % '~'
-define(KITTY_END, 117). % 'u'
-define(SEMICOLON, 59).  % ';'
-define(SPACE, 32).
-define(BACKSPACE, 8).
-define(TAB, 9).
-define(ENTER, 13).
-define(BACKSPACE2, 127).


-define(RESET,              [?ESC, ?CSI_START, "0m"]).
-define(ALT_SCREEN_ENABLE,  [?ESC, ?CSI_START, "1049h"]).
-define(ALT_SCREEN_DISABLE, [?ESC, ?CSI_START, "1049l"]).

-define(SYNC_UPDATE_ENABLE,  [?ESC, ?CSI_START, "?2026h"]).
-define(SYNC_UPDATE_DISABLE, [?ESC, ?CSI_START, "?2026l"]).

-define(SHOW_CURSOR, [?ESC, ?CSI_START, "25h"]).

-define(HIDE_CURSOR, "\033[?25l").
-define(HIDE_CURSOR_WUT, [?ESC, ?CSI_START, "25l"]).

-define(CLEAR, [?ESC, ?CSI_START, "2J", ?ESC, ?CSI_START, "H"]). 

% -define(ALT_SCREEN_ENABLE,  "\e[?1049h").
% -define(ALT_SCREEN_DISABLE, "\e[?1049l").
% \033[?25l
% ESC[?25l
% -define(HIDE_CURSOR, "\e[?25l").
% -define(SHOW_CURSOR, "\e[?25h").


%% ASCII ranges
-define(DIGIT_0, 48).
-define(DIGIT_1, 49).
-define(DIGIT_2, 50).
-define(DIGIT_3, 51).
-define(DIGIT_4, 52).
-define(DIGIT_5, 53).
-define(DIGIT_6, 54).
-define(DIGIT_7, 55).
-define(DIGIT_8, 56).
-define(DIGIT_9, 57).

-define(UPPER_A, 65).
-define(UPPER_Z, 90).
-define(LOWER_A, 97).
-define(LOWER_Z, 122).

-define(PRINTABLE_START, 33).
-define(PRINTABLE_END, 126).

%% Special sequence characters
-define(CHAR_H, 72).  % 'H' for Home
-define(CHAR_F, 70).  % 'F' for End
-define(CHAR_A, 65).  % 'A' for Up
-define(CHAR_B, 66).  % 'B' for Down
-define(CHAR_C, 67).  % 'C' for Right
-define(CHAR_D, 68).  % 'D' for Left
-define(CHAR_P, 80).  % 'P' for F1
-define(CHAR_Q, 81).  % 'Q' for F2
-define(CHAR_R, 82).  % 'R' for F3
-define(CHAR_S, 83).  % 'S' for F4



-define(SERVER, ?MODULE).

%% ===================================================================
%% Public API
%% ===================================================================

-doc "Initializes the terminal. Enables the alternate screen buffer and hides the cursor.".
tb_init() ->
    case whereis(?SERVER) of
        undefined ->
            {ok, _} = start_link();
        _ ->
            ok
    end,
    gen_server:call(?SERVER, init_term).

-doc "Shuts down the terminal. Resets attributes, shows the cursor, and disables the alternate screen buffer.".
tb_shutdown() ->
    gen_server:call(?SERVER, shutdown_term).

-doc "Returns the width of the terminal in columns.".
tb_width() ->
    gen_server:call(?SERVER, get_width).

-doc "Returns the height of the terminal in rows.".
tb_height() ->
    gen_server:call(?SERVER, get_height).

-doc "Clears the entire terminal screen.".
tb_clear() ->
    gen_server:call(?SERVER, tb_clear).

-doc "Flushes pending output to the terminal using synchronized updates if supported.".
tb_present() ->
    gen_server:call(?SERVER, tb_present).

-doc "Sets a single character at the specified (X, Y) position with given foreground and background colors.".
tb_set_cell(X, Y, Char, Fg, Bg) ->
    gen_server:call(?SERVER, {tb_set_cell, X, Y, Char, Fg, Bg}).


-doc "Prints a string at the specified (X, Y) position with given foreground and background colors.".
tb_print(X, Y, Fg, Bg, Str) ->
    gen_server:call(?SERVER, {tb_print, X, Y, Fg, Bg, Str}).

-doc "Returns the character and colors at the specified (X, Y) position from the internal shadow buffer.".
get_cell(X, Y) ->
    gen_server:call(?SERVER, {get_cell, X, Y}).

-doc "Returns an entire row of cells.".
tb_get_row(Y, Width) ->
    gen_server:call(?SERVER, {get_row, Y, Width}).

-doc "Returns a segment of a row of cells.".
tb_get_row(X, Y, Length) ->
    gen_server:call(?SERVER, {get_row, X, Y, Length}).

-doc "Polls for a terminal event. This function blocks until an event occurs.".
tb_poll_event() ->
    gen_server:call(?SERVER, get_event, infinity).

-doc "Alias for tb_poll_event/0. Polls for a terminal event.".
get_next_event() ->
    gen_server:call(?SERVER, get_event, infinity).

-doc "Sets the input mode for the terminal. Mode can be 'default' or 'alt'.".
tb_set_input_mode(Mode) ->
    gen_server:call(?SERVER, {set_input_mode, Mode}).

-doc "Sets the output mode for the terminal. Mode can be 'default', 2 (256-color) 4, or 5 (truecolor).".
tb_set_output_mode(Mode) ->
    gen_server:call(?SERVER, {tb_set_output_mode, Mode}).

-doc "Forces a full redraw on the next tb_present call by clearing the front buffer.".
tb_force_redraw() ->
    gen_server:call(?SERVER, tb_force_redraw).

%% ===================================================================
%% Server implementation
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

init([]) ->
    os:set_signal(sigwinch, handle),
    {ok, #state{width=0, height=0, input_mode=alt, output_mode=default, back_buffer = cellium_buffer:empty(), front_buffer = cellium_buffer:empty()}}.

handle_call(init_term, _From, State) ->
    logger:debug("INIT TERM FROM NATIVE TERMINAL"), 
    shell:start_interactive({noshell, raw}),
    io:put_chars(?ALT_SCREEN_ENABLE), % Enable alternate screen buffer
    io:put_chars(?HIDE_CURSOR),
    io:put_chars("\e[2J"),           % Initial clear
    {ok, W} = io:columns(),
    {ok, H} = io:rows(),
    start_event_loop(),
    {reply, ok, State#state{width=W, height=H, back_buffer = cellium_buffer:empty(), front_buffer = cellium_buffer:empty()}};

handle_call(shutdown_term, _From, State) ->
    io:put_chars(?RESET),              % Reset all attributes
    io:put_chars(?SHOW_CURSOR),        % Show the cursor
    io:put_chars(?SYNC_UPDATE_DISABLE), % Disable synchronized updates
    io:put_chars(?ALT_SCREEN_DISABLE), % Disable alternate screen buffer
    {reply, ok, State};

handle_call(tb_clear, _From, State) ->
    {reply, ok, State#state{back_buffer = cellium_buffer:empty()}};

handle_call(tb_present, _From, State) ->
    #state{back_buffer = Back, front_buffer = Front, output_mode = OutputMode} = State,

    % Start synchronized update
    io:put_chars(?SYNC_UPDATE_ENABLE),

    % Calculate and send diff
    DiffOutput = diff_and_draw(Back, Front, OutputMode),
    io:put_chars(DiffOutput),

    % End synchronized update
    io:put_chars(?SYNC_UPDATE_DISABLE),

    {reply, ok, State#state{front_buffer = Back}};

handle_call({tb_set_cell, X, Y, Char, Fg, Bg}, _From, State) ->
    NewBuffer = cellium_buffer:set_cell(X, Y, Char, Fg, Bg, State#state.back_buffer),
    {reply, ok, State#state{back_buffer = NewBuffer}};

handle_call({tb_print, X, Y, Fg, Bg, Str}, _From, State) ->
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, Str, State#state.back_buffer),
    {reply, ok, State#state{back_buffer = NewBuffer}};

handle_call({get_cell, X, Y}, _From, State) ->
    {reply, cellium_buffer:get_cell(X, Y, State#state.back_buffer), State};

handle_call({get_row, Y, Width}, _From, State) ->
    {reply, cellium_buffer:get_row(Y, Width, State#state.back_buffer), State};

handle_call({get_row, X, Y, Length}, _From, State) ->
    {reply, cellium_buffer:get_row(X, Y, Length, State#state.back_buffer), State};

handle_call(get_width, _From, State) ->
    {ok, W} = io:columns(),
    {reply, W, State};

handle_call(get_height, _From, State) ->
    {ok, Cols} = io:rows(),
    {reply, Cols, State};

handle_call({set_input_mode, Mode}, _From, State) ->
    {reply, ok, State#state{input_mode = Mode}};

handle_call({set_output_mode, Mode}, _From, State) ->

   OutputMode  = case Mode of
                     2 -> default;
                     4 -> 256;
                     5 -> truecolor;
                     _ -> truecolor
                 end,

    logger:info("Setting output mode: ~p", [OutputMode]),
    {reply, ok, State#state{output_mode = OutputMode}};

handle_call(get_output_mode, _From, State) ->
    {reply, State#state.output_mode, State};

handle_call(get_event, From, State) ->
    case State#state.event_buffer of
        [Event | Rest] ->
            {reply, Event, State#state{event_buffer = Rest}};
        [] ->
            {noreply, State#state{waiting_client = From}}
    end;

handle_call(tb_force_redraw, _From, State) ->
    io:put_chars(?CLEAR),
    {reply, ok, State#state{front_buffer = cellium_buffer:empty()}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, not_supported}, State}.

handle_cast({raw_char, Char}, State) ->
    NewState = process_char(Char, State),
    {noreply, NewState};

handle_cast(check_buffer_timeout, State = #state{input_mode = InputMode}) ->
    case State#state.char_buffer of
        [] ->
            {noreply, State};
        Buffer ->
            Now = erlang:monotonic_time(millisecond),
            case State#state.buffer_time of
                undefined ->
                    {noreply, State};
                BufferTime when Now - BufferTime > 100 ->
                    case try_parse_sequence(Buffer) of
                        {incomplete} ->
                            {noreply, State};
                        _ ->
                            Events = lists:map(fun(C) -> parse_single_char(C, InputMode) end, Buffer),
                            NewState = lists:foldl(fun add_event/2, State, Events),
                            {noreply, NewState#state{char_buffer = [], buffer_time = undefined}}
                    end;
                _ ->
                    {noreply, State}
            end
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({signal, sigwinch}, State) ->
    {ok, Cols} = io:columns(),
    {ok, Rows} = io:rows(),
    W = max(1, Cols),
    H = max(1, Rows),
    logger:info("Terminal resize detected via SIGNAL: ~p x ~p", [W, H]),
    % Physically clear the screen as resize often scrambles contents
    io:put_chars(?CLEAR),
    % Force full redraw on next update
    NewState = State#state{width = W, height = H, front_buffer = cellium_buffer:empty(), back_buffer = cellium_buffer:empty()},
    % Notify event manager
    Event = {resize, W, H},
    FinalState = add_event(Event, NewState),
    {noreply, FinalState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

diff_and_draw(Back, Front, OutputMode) ->
    % Keys that are in Back
    AllBackKeys = maps:keys(Back),
    % Keys that were in Front but are now gone (need to be cleared)
    RemovedKeys = maps:keys(maps:without(AllBackKeys, Front)),
    
    % Changes and additions
    DrawOps = maps:fold(fun(K, V, Acc) ->
        case maps:get(K, Front, undefined) of
            V -> Acc; % No change
            _ -> [render_cell(K, V, OutputMode) | Acc]
        end
    end, [], Back),
    
    % Deletions
    ClearOps = lists:foldl(fun(K, Acc) ->
        [render_cell(K, {$\s, default, default}, OutputMode) | Acc]
    end, [], RemovedKeys),
    
    DrawOps ++ ClearOps.

render_cell({X, Y}, {Char, Fg, Bg}, OutputMode) ->
    FgAnsi = lookup_color(Fg, fg, OutputMode),
    BgAnsi = lookup_color(Bg, bg, OutputMode),
    [
        "\e[", integer_to_list(Y + 1), ";", integer_to_list(X + 1), "H",
        FgAnsi,
        BgAnsi,
        [Char],
        ?RESET
    ].

start_event_loop() ->
    spawn(fun() -> event_loop() end).

schedule_timeout_check() ->
    spawn(fun() ->
        timer:sleep(100),
        gen_server:cast(?SERVER, check_buffer_timeout)
    end).

event_loop() ->
    case io:get_chars("", 1) of
        eof ->
            gen_server:cast(?SERVER, {raw_char, eof}),
            event_loop();
        [Char] ->
            gen_server:cast(?SERVER, {raw_char, Char}),
            timer:sleep(1),
            event_loop();
        _ ->
            event_loop()
    end.

process_char(eof, State) ->
    Event = {event, eof, 0, 0, 0, 0},
    add_event(Event, State);

process_char(?ESC, State) ->
    schedule_timeout_check(),
    State#state{
        char_buffer = [?ESC],
        buffer_time = erlang:monotonic_time(millisecond)
    };

process_char(Char, #state{char_buffer = [], input_mode = InputMode} = State) ->
    Event = parse_single_char(Char, InputMode),
    add_event(Event, State);

process_char(Char, #state{char_buffer = Buffer, input_mode = InputMode} = State) ->
    NewBuffer = Buffer ++ [Char],
    case try_parse_sequence(NewBuffer) of
        {complete, Event} ->
            NewState = add_event(Event, State),
            NewState#state{char_buffer = [], buffer_time = undefined};
        {incomplete} ->
            schedule_timeout_check(),
            State#state{
                char_buffer = NewBuffer,
                buffer_time = erlang:monotonic_time(millisecond)
            };
        {invalid} ->
            Events = lists:map(fun(C) -> parse_single_char(C, InputMode) end, NewBuffer),
            NewState = lists:foldl(fun add_event/2, State, Events),
            NewState#state{char_buffer = [], buffer_time = undefined}
    end.

add_event(Event, #state{event_buffer = Buffer, waiting_client = undefined} = State) ->
    State#state{event_buffer = Buffer ++ [Event]};

add_event(Event, #state{waiting_client = Client} = State) ->
    %% what ?
    %% Client is {Pid, Reference}
    gen_server:reply(Client, Event),
    State#state{waiting_client = undefined}.
 

parse_single_char(Char, alt) ->
    case keyboard_maps:parse_alt_char(Char) of
        nomatch -> parse_single_char(Char, default);
        Event -> Event
    end;

parse_single_char(0, _) -> {key, false, false, true, false, <<"~\\">>};  % CTRL_TILDE / CTRL_2
parse_single_char(1, _) -> {key, false, false, true, false, <<"a">>};
parse_single_char(2, _) -> {key, false, false, true, false, <<"b">>};
parse_single_char(3, _) -> {key, false, false, true, false, <<"c">>};
parse_single_char(4, _) -> {key, false, false, true, false, <<"d">>};
parse_single_char(5, _) -> {key, false, false, true, false, <<"e">>};
parse_single_char(6, _) -> {key, false, false, true, false, <<"f">>};
parse_single_char(7, _) -> {key, false, false, true, false, <<"g">>};
parse_single_char(?BACKSPACE, _) -> {key, false, false, false, false, backspace_key};  % also CTRL_H
parse_single_char(?TAB, _) -> {key, false, false, false, false, tab_key};  % also CTRL_I
parse_single_char(10, _) -> {key, false, false, true, false, <<"j">>};
parse_single_char(11, _) -> {key, false, false, true, false, <<"k">>};
parse_single_char(12, _) -> {key, false, false, true, false, <<"l">>};
parse_single_char(?ENTER, _) -> {key, false, false, false, false, enter_key};  % also CTRL_M
parse_single_char(14, _) -> {key, false, false, true, false, <<"n">>};
parse_single_char(15, _) -> {key, false, false, true, false, <<"o">>};
parse_single_char(16, _) -> {key, false, false, true, false, <<"p">>};
parse_single_char(17, _) -> {key, false, false, true, false, <<"q">>};
parse_single_char(18, _) -> {key, false, false, true, false, <<"r">>};
parse_single_char(19, _) -> {key, false, false, true, false, <<"s">>};
parse_single_char(20, _) -> {key, false, false, true, false, <<"t">>};
parse_single_char(21, _) -> {key, false, false, true, false, <<"u">>};
parse_single_char(22, _) -> {key, false, false, true, false, <<"v">>};
parse_single_char(23, _) -> {key, false, false, true, false, <<"w">>};
parse_single_char(24, _) -> {key, false, false, true, false, <<"x">>};
parse_single_char(25, _) -> {key, false, false, true, false, <<"y">>};
parse_single_char(26, _) -> {key, false, false, true, false, <<"z">>};
parse_single_char(?ESC, _) -> {key, false, false, false, false, esc_key};  % also CTRL_[ / CTRL_3
parse_single_char(28, _) -> {key, false, false, true, false, <<"\\">>};  % CTRL_4 / CTRL_BACKSLASH
parse_single_char(29, _) -> {key, false, false, true, false, <<"]">>};  % CTRL_5 / CTRL_RSQ_BRACKET
parse_single_char(30, _) -> {key, false, false, true, false, <<"6">>};
parse_single_char(31, _) -> {key, false, false, true, false, <<"/">>};  % CTRL_7 / CTRL_SLASH / CTRL_UNDERSCORE
parse_single_char(?SPACE, _) -> {key, false, false, false, false, <<" ">>};
parse_single_char(?BACKSPACE2, _) -> {key, false, false, false, false, backspace2_key};
parse_single_char(C, _) when C >= ?PRINTABLE_START, C =< ?PRINTABLE_END ->
    {key, false, false, false, false, unicode:characters_to_binary([C])};
parse_single_char(C, _) ->
    {char, C}.

try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_3, ?TILDE]) -> {complete, {key, false, false, false, false, delete_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_5, ?TILDE]) -> {complete, {key, false, false, false, false, pgup_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_6, ?TILDE]) -> {complete, {key, false, false, false, false, pgdn_key}};
try_parse_sequence([?ESC, ?CSI_START, ?CHAR_H]) -> {complete, {key, false, false, false, false, home_key}};
try_parse_sequence([?ESC, ?CSI_START, ?CHAR_F]) -> {complete, {key, false, false, false, false, end_key}};
try_parse_sequence([?ESC, ?CSI_START, ?CHAR_A]) -> {complete, {key, false, false, false, false, up_key}};
try_parse_sequence([?ESC, ?CSI_START, ?CHAR_B]) -> {complete, {key, false, false, false, false, down_key}};
try_parse_sequence([?ESC, ?CSI_START, ?CHAR_C]) -> {complete, {key, false, false, false, false, right_key}};
try_parse_sequence([?ESC, ?CSI_START, ?CHAR_D]) -> {complete, {key, false, false, false, false, left_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_A]) -> {complete, {key, false, false, false, false, up_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_B]) -> {complete, {key, false, false, false, false, down_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_C]) -> {complete, {key, false, false, false, false, right_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_D]) -> {complete, {key, false, false, false, false, left_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_P]) -> {complete, {key, false, false, false, false, f1_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_Q]) -> {complete, {key, false, false, false, false, f2_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_R]) -> {complete, {key, false, false, false, false, f3_key}};
try_parse_sequence([?ESC, ?SS3_START, ?CHAR_S]) -> {complete, {key, false, false, false, false, f4_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_5, ?TILDE]) -> {complete, {key, false, false, false, false, f5_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_7, ?TILDE]) -> {complete, {key, false, false, false, false, f6_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_8, ?TILDE]) -> {complete, {key, false, false, false, false, f7_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_9, ?TILDE]) -> {complete, {key, false, false, false, false, f8_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_0, ?TILDE]) -> {complete, {key, false, false, false, false, f9_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_1, ?TILDE]) -> {complete, {key, false, false, false, false, f10_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_3, ?TILDE]) -> {complete, {key, false, false, false, false, f11_key}};
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_4, ?TILDE]) -> {complete, {key, false, false, false, false, f12_key}};
try_parse_sequence([?ESC, ?CSI_START, 90]) -> {complete, {key, true, false, false, false, tab_key}}; %% Shift + Tab
try_parse_sequence([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_7, ?SEMICOLON, ?DIGIT_5, ?SEMICOLON, ?DIGIT_1, ?DIGIT_3, ?TILDE]) -> {complete, {key, false, false, true, false, enter_key}};
try_parse_sequence([?ESC, C]) when C >= ?LOWER_A, C =< ?LOWER_Z ->  % ESC + lowercase letter (alt+letter)
    {complete, {key, false, true, false, false, <<C>>}};
try_parse_sequence([?ESC, C]) when C >= ?UPPER_A, C =< ?UPPER_Z ->  % ESC + uppercase letter (alt+shift+letter)
    {complete, {key, true, true, false, false, <<(C + 32)>>}};
try_parse_sequence([?ESC, C]) when C >= ?DIGIT_0, C =< ?DIGIT_9 ->  % ESC + digit (alt+number)
    {complete, {key, false, true, false, false, <<C>>}};
try_parse_sequence(Seq) ->
    case parse_kitty_sequence(Seq) of
        {complete, _} = Result -> Result;
        _ -> check_incomplete_or_invalid(Seq)
    end.

parse_kitty_sequence([?ESC, ?CSI_START | Rest]) ->
    case Rest of
        [] -> incomplete_or_invalid;
        _ ->
            case lists:last(Rest) of
                ?KITTY_END ->  % ends with 'u'
                    Params = lists:sublist(Rest, length(Rest) - 1),
                    case parse_kitty_params(Params) of
                        {Keycode, ModifierBits} ->
                            {Shift, Alt, Control, Meta, Key} = kitty_keycode_to_key(Keycode, ModifierBits),
                            {complete, {key, Shift, Alt, Control, Meta, Key}};
                        _ ->
                            incomplete_or_invalid
                    end;
                _ ->
                    incomplete_or_invalid
            end
    end;
parse_kitty_sequence(_) ->
    incomplete_or_invalid.

parse_kitty_params(Params) ->
    ParamStr = lists:map(fun(C) -> C end, Params),
    case string:split(ParamStr, ";", all) of
        [KeycodeStr, ModStr] ->
            try
                Keycode = list_to_integer(KeycodeStr),
                Mods = list_to_integer(ModStr),
                {Keycode, Mods}
            catch
                _:_ -> invalid
            end;
        _ ->
            invalid
    end.

kitty_keycode_to_key(Keycode, ModifierBits) ->
    Shift = (ModifierBits band 1) =/= 0,
    Alt = (ModifierBits band 2) =/= 0,
    Control = (ModifierBits band 4) =/= 0,
    Meta = (ModifierBits band 8) =/= 0,
    
    Key = if
        Control andalso Keycode >= ?LOWER_A andalso Keycode =< ?LOWER_Z ->
            <<(Keycode - 32)>>;
        true ->
            unicode:characters_to_binary([Keycode])
    end,
    
    {Shift, Alt, Control, Meta, Key}.

check_incomplete_or_invalid([?ESC]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?SS3_START]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_3]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_5]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_6]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_1]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_5]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_7]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_8]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_1, ?DIGIT_9]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_0]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_1]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_3]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_4]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_7]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_7, ?SEMICOLON]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_7, ?SEMICOLON, ?DIGIT_5]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_7, ?SEMICOLON, ?DIGIT_5, ?SEMICOLON]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_7, ?SEMICOLON, ?DIGIT_5, ?SEMICOLON, ?DIGIT_1]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START, ?DIGIT_2, ?DIGIT_7, ?SEMICOLON, ?DIGIT_5, ?SEMICOLON, ?DIGIT_1, ?DIGIT_3]) -> {incomplete};
check_incomplete_or_invalid([?ESC, ?CSI_START | Rest]) ->
    case Rest of
        [] -> {incomplete};
        _ ->
            LastChar = lists:last(Rest),
            case LastChar of
                ?KITTY_END -> {incomplete};  % ends with 'u', might be incomplete kitty
                _ -> check_if_partial_kitty(Rest)
            end
    end;
check_incomplete_or_invalid(_) -> {invalid}.

check_if_partial_kitty(Rest) ->
    HasSemicolon = lists:any(fun(C) -> C =:= ?SEMICOLON end, Rest),
    HasDigit = lists:any(fun(C) -> C >= ?DIGIT_0 andalso C =< ?DIGIT_9 end, Rest),
    case HasSemicolon orelse HasDigit of
        true -> {incomplete};  % likely partial kitty sequence
        false -> {invalid}
    end.


lookup_color(Color, _Type, default) when is_list(Color) ->
    erlang:error(#{reason => "NOT IMPLEMENTED",
                   line => ?LINE});

lookup_color(Color, _Type, normal) when is_list(Color) ->
    erlang:error(#{reason => "NOT IMPLEMENTED",
                   line => ?LINE});

lookup_color(Color, Type, truecolor) when is_list(Color) ->

    HexBin = list_to_binary(Color),
    <<RR:2/binary, GG:2/binary, BB:2/binary>> = HexBin,

    R = list_to_integer(binary_to_list(RR), 16),
    G = list_to_integer(binary_to_list(GG), 16),
    B = list_to_integer(binary_to_list(BB), 16),

    EscCode =
        case Type of
            fg ->
                truecolor_fg(R,G,B);
            bg ->
                truecolor_bg(R, G, B)
        end,

    EscCode;

lookup_color(Color, Type, OutputMode) when is_integer(Color) ->
    R = (Color bsr 16) band 16#FF,
    G = (Color bsr 8) band 16#FF,
    B = Color band 16#FF,
    lookup_color({R,G,B}, Type, OutputMode);

lookup_color({R,G,B}, fg, 5) -> truecolor_fg(R,G,B);
lookup_color({R,G,B}, bg, 5) -> truecolor_bg(R,G,B);

lookup_color({R,G,B}, fg, 2) -> ansi256_fg(rgb_to_ansi256(R,G,B));
lookup_color({R,G,B}, bg, 2) -> ansi256_bg(rgb_to_ansi256(R,G,B));

lookup_color(default, fg, _) -> [27, 91, 51, 57, 109];
lookup_color(black, fg, _) -> [27, 91, 51, 48, 109];
lookup_color(red, fg, _) -> [27, 91, 51, 49, 109];
lookup_color(green, fg, _) -> [27, 91, 51, 50, 109];
lookup_color(yellow, fg, _) -> [27, 91, 51, 51, 109];
lookup_color(blue, fg, _) -> [27, 91, 51, 52, 109];
lookup_color(magenta, fg, _) -> [27, 91, 51, 53, 109];
lookup_color(cyan, fg, _) -> [27, 91, 51, 54, 109];
lookup_color(white, fg, _) -> [27, 91, 51, 55, 109];
lookup_color(bright_black, fg, _) -> [27, 91, 57, 48, 109];
lookup_color(bright_red, fg, _) -> [27, 91, 57, 49, 109];
lookup_color(bright_green, fg, _) -> [27, 91, 57, 50, 109];
lookup_color(bright_yellow, fg, _) -> [27, 91, 57, 51, 109];
lookup_color(bright_blue, fg, _) -> [27, 91, 57, 52, 109];
lookup_color(bright_magenta, fg, _) -> [27, 91, 57, 53, 109];
lookup_color(bright_cyan, fg, _) -> [27, 91, 57, 54, 109];
lookup_color(bright_white, fg, _) -> [27, 91, 57, 55, 109];

lookup_color(default, bg, _) -> [27, 91, 52, 57, 109];
lookup_color(black, bg, _) -> [27, 91, 52, 48, 109];
lookup_color(red, bg, _) -> [27, 91, 52, 49, 109];
lookup_color(green, bg, _) -> [27, 91, 52, 50, 109];
lookup_color(yellow, bg, _) -> [27, 91, 52, 51, 109];
lookup_color(blue, bg, _) -> [27, 91, 52, 52, 109];
lookup_color(magenta, bg, _) -> [27, 91, 52, 53, 109];
lookup_color(cyan, bg, _) -> [27, 91, 52, 54, 109];
lookup_color(white, bg, _) -> [27, 91, 52, 55, 109];
lookup_color(bright_black, bg, _) -> [27, 91, 49, 48, 48, 109];
lookup_color(bright_red, bg, _) -> [27, 91, 49, 48, 49, 109];
lookup_color(bright_green, bg, _) -> [27, 91, 49, 48, 50, 109];
lookup_color(bright_yellow, bg, _) -> [27, 91, 49, 48, 51, 109];
lookup_color(bright_blue, bg, _) -> [27, 91, 49, 48, 52, 109];
lookup_color(bright_magenta, bg, _) -> [27, 91, 49, 48, 53, 109];
lookup_color(bright_cyan, bg, _) -> [27, 91, 49, 48, 54, 109];
lookup_color(bright_white, bg, _) -> [27, 91, 49, 48, 55, 109];
lookup_color(_, _, _) ->
    "".

truecolor_fg(R, G, B) ->
    [$\e, "[38;2;", integer_to_list(R), ";", integer_to_list(G), ";", integer_to_list(B), "m"].

truecolor_bg(R, G, B) ->
    [$\e, "[48;2;", integer_to_list(R), ";", integer_to_list(G), ";", integer_to_list(B), "m"].

ansi256_fg(Color) ->
    [$\e, "[38;5;", integer_to_list(Color), "m"].

ansi256_bg(Color) ->
    [$\e, "[48;5;", integer_to_list(Color), "m"].

rgb_to_ansi256(R, G, B) when R =:= G, G =:= B ->
    if
        R < 8 -> 16;
        R > 248 -> 231;
        true ->
            round((R - 8) / 10) + 232
    end;

rgb_to_ansi256(R, G, B) ->
    16 +
    (36 * round(R / 255 * 5)) +
    (6 * round(G / 255 * 5)) +
    round(B / 255 * 5).
