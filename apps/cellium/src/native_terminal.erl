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
    tb_poll_event/0,
    get_next_event/0,
    tb_set_input_mode/1,
    tb_set_output_mode/1
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
    waiting_client = undefined :: undefined | pid()
}).

-define(RESET, "\e[0m").

-define(ALT_SCREEN_ENABLE,  "\e[?1049h").
-define(ALT_SCREEN_DISABLE, "\e[?1049l").

-define(HIDE_CURSOR, "\e[?25l").
-define(SHOW_CURSOR, "\e[?25h").

-doc """
  Represents the state of the native terminal driver.

  @field width The width of the terminal in columns.
  @field height The height of the terminal in rows.
  @field input_mode The current input mode (e.g., `default`, `alt`).
  @field output_mode The current output mode (e.g., `2` for 256-color, `5` for truecolor).
  @field char_buffer Buffer for incoming characters to parse escape sequences.
  @field event_buffer Buffer for parsed events waiting to be consumed.
  @field buffer_time Timestamp for managing character buffer timeouts.
  @field waiting_client The PID of a client waiting for an event.
""".

-type state() :: #state{}.

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
    io:put_chars("\e[2J").

-doc "No-op for now, but would typically flush pending output to the terminal.".
tb_present() ->
    ok.

-doc "Sets a single character at the specified (X, Y) position with given foreground and background colors.".
tb_set_cell(X, Y, Char, Fg, Bg) ->
    OutputMode = tb_get_output_mode(),
    FgAnsi = lookup_color(Fg, fg, OutputMode),
    BgAnsi = lookup_color(Bg, bg, OutputMode),
    io:put_chars([
        "\e[" ++ integer_to_list(Y + 1) ++ ";" ++ integer_to_list(X + 1) ++ "H",
        FgAnsi,
        BgAnsi,
        [Char],
        ?RESET
    ]).

-doc "Prints a string at the specified (X, Y) position with given foreground and background colors.".
tb_print(X, Y, Fg, Bg, Str) ->
    OutputMode = tb_get_output_mode(),

    FgAnsi = lookup_color(Fg, fg, OutputMode),
    BgAnsi = lookup_color(Bg, bg, OutputMode),


    Move = "\e[" ++ integer_to_list(Y + 1) ++ ";" ++ integer_to_list(X + 1) ++ "H",
    io:put_chars([Move, FgAnsi, BgAnsi, Str, ?RESET]).

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
    gen_server:call(?SERVER, {set_output_mode, Mode}).

-doc "Returns the current output mode of the terminal.".
tb_get_output_mode() ->
    gen_server:call(?SERVER, get_output_mode).

%% ===================================================================
%% Server implementation
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

init([]) ->
    {ok, #state{width=0, height=0, input_mode=alt, output_mode=default}}.

handle_call(init_term, _From, State) ->
    shell:start_interactive({noshell, raw}),
    io:put_chars(?ALT_SCREEN_ENABLE), % Enable alternate screen buffer
    io:put_chars(?HIDE_CURSOR),
    {ok, W} = io:columns(),
    {ok, H} = io:rows(),
    start_event_loop(),
    {reply, ok, State#state{width=W, height=H}};

handle_call(shutdown_term, _From, State) ->
    io:put_chars(?RESET),              % Reset all attributes
    io:put_chars(?SHOW_CURSOR),        % Show the cursor
    io:put_chars(?ALT_SCREEN_DISABLE), % Disable alternate screen buffer
    {reply, ok, State};

handle_call(get_width, _From, State) ->
    {reply, State#state.width, State};

handle_call(get_height, _From, State) ->
    {reply, State#state.height, State};

handle_call({set_input_mode, Mode}, _From, State) ->
    {reply, ok, State#state{input_mode = Mode}};

handle_call({set_output_mode, Mode}, _From, State) ->

   OutputMode  = case Mode of
                     2 -> default;
                     4 -> 256;
                     5 -> truecolor;
                     _ -> truecolor
                 end,

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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

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

process_char(27, State) ->
    schedule_timeout_check(),
    State#state{
        char_buffer = [27],
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
    gen_server:reply(Client, Event),
    State#state{waiting_client = undefined}.

parse_single_char(Char, alt) ->
    case keyboard_maps:parse_alt_char(Char) of
        nomatch -> parse_single_char(Char, default);
        Event -> Event
    end;

parse_single_char(0, _) -> {key, [ctrl_key], <<"~\\">>};  % CTRL_TILDE / CTRL_2
parse_single_char(1, _) -> {key, [ctrl_key], <<"a">>};
parse_single_char(2, _) -> {key, [ctrl_key], <<"b">>};
parse_single_char(3, _) -> {key, [ctrl_key], <<"c">>};
parse_single_char(4, _) -> {key, [ctrl_key], <<"d">>};
parse_single_char(5, _) -> {key, [ctrl_key], <<"e">>};
parse_single_char(6, _) -> {key, [ctrl_key], <<"f">>};
parse_single_char(7, _) -> {key, [ctrl_key], <<"g">>};
parse_single_char(8, _) -> {key, [], backspace_key};  % also CTRL_H
parse_single_char(9, _) -> {key, [], tab_key};  % also CTRL_I
parse_single_char(10, _) -> {key, [ctrl_key], <<"j">>};
parse_single_char(11, _) -> {key, [ctrl_key], <<"k">>};
parse_single_char(12, _) -> {key, [ctrl_key], <<"l">>};
parse_single_char(13, _) -> {key, [], enter_key};  % also CTRL_M
parse_single_char(14, _) -> {key, [ctrl_key], <<"n">>};
parse_single_char(15, _) -> {key, [ctrl_key], <<"o">>};
parse_single_char(16, _) -> {key, [ctrl_key], <<"p">>};
parse_single_char(17, _) -> {key, [ctrl_key], <<"q">>};
parse_single_char(18, _) -> {key, [ctrl_key], <<"r">>};
parse_single_char(19, _) -> {key, [ctrl_key], <<"s">>};
parse_single_char(20, _) -> {key, [ctrl_key], <<"t">>};
parse_single_char(21, _) -> {key, [ctrl_key], <<"u">>};
parse_single_char(22, _) -> {key, [ctrl_key], <<"v">>};
parse_single_char(23, _) -> {key, [ctrl_key], <<"w">>};
parse_single_char(24, _) -> {key, [ctrl_key], <<"x">>};
parse_single_char(25, _) -> {key, [ctrl_key], <<"y">>};
parse_single_char(26, _) -> {key, [ctrl_key], <<"z">>};
parse_single_char(27, _) -> {key, [], esc_key};  % also CTRL_[ / CTRL_3
parse_single_char(28, _) -> {key, [ctrl_key], <<"\\">>};  % CTRL_4 / CTRL_BACKSLASH
parse_single_char(29, _) -> {key, [ctrl_key], <<"]">>};  % CTRL_5 / CTRL_RSQ_BRACKET
parse_single_char(30, _) -> {key, [ctrl_key], <<"6">>};
parse_single_char(31, _) -> {key, [ctrl_key], <<"/">>};  % CTRL_7 / CTRL_SLASH / CTRL_UNDERSCORE
parse_single_char(32, _) -> {key, [], <<" ">>};
parse_single_char(127, _) -> {key, [], backspace2_key};
parse_single_char(C, _) when C >= 33, C =< 126 ->
    {key, [], unicode:characters_to_binary([C])};
parse_single_char(C, _) ->
    {char, C}.

try_parse_sequence([27, 91, 51, 126]) -> {complete, {key, [], delete_key}};
try_parse_sequence([27, 91, 53, 126]) -> {complete, {key, [], pgup_key}};
try_parse_sequence([27, 91, 54, 126]) -> {complete, {key, [], pgdn_key}};
try_parse_sequence([27, 91, 72]) -> {complete, {key, [], home_key}};
try_parse_sequence([27, 91, 70]) -> {complete, {key, [], end_key}};
try_parse_sequence([27, 91, 65]) -> {complete, {key, [], up_key}};
try_parse_sequence([27, 91, 66]) -> {complete, {key, [], down_key}};
try_parse_sequence([27, 91, 67]) -> {complete, {key, [], right_key}};
try_parse_sequence([27, 91, 68]) -> {complete, {key, [], left_key}};
try_parse_sequence([27, 79, 65]) -> {complete, {key, [], up_key}};
try_parse_sequence([27, 79, 66]) -> {complete, {key, [], down_key}};
try_parse_sequence([27, 79, 67]) -> {complete, {key, [], right_key}};
try_parse_sequence([27, 79, 68]) -> {complete, {key, [], left_key}};
try_parse_sequence([27, 79, 80]) -> {complete, {key, [], f1_key}};
try_parse_sequence([27, 79, 81]) -> {complete, {key, [], f2_key}};
try_parse_sequence([27, 79, 82]) -> {complete, {key, [], f3_key}};
try_parse_sequence([27, 79, 83]) -> {complete, {key, [], f4_key}};
try_parse_sequence([27, 91, 49, 53, 126]) -> {complete, {key, [], f5_key}};
try_parse_sequence([27, 91, 49, 55, 126]) -> {complete, {key, [], f6_key}};
try_parse_sequence([27, 91, 49, 56, 126]) -> {complete, {key, [], f7_key}};
try_parse_sequence([27, 91, 49, 57, 126]) -> {complete, {key, [], f8_key}};
try_parse_sequence([27, 91, 50, 48, 126]) -> {complete, {key, [], f9_key}};
try_parse_sequence([27, 91, 50, 49, 126]) -> {complete, {key, [], f10_key}};
try_parse_sequence([27, 91, 50, 51, 126]) -> {complete, {key, [], f11_key}};
try_parse_sequence([27, 91, 50, 52, 126]) -> {complete, {key, [], f12_key}};
try_parse_sequence([27, 91, 50, 55, 59, 53, 59, 49, 51, 126]) -> {complete, {key, [ctrl_key], enter_key}};
try_parse_sequence([27, C]) when C >= 97, C =< 122 ->  % ESC + lowercase letter (alt+letter)
    {complete, {key, [alt_key], <<C>>}};
try_parse_sequence([27, C]) when C >= 65, C =< 90 ->  % ESC + uppercase letter (alt+shift+letter)
    {complete, {key, [alt_key, shift_key], <<(C + 32)>>}};
try_parse_sequence([27, C]) when C >= 48, C =< 57 ->  % ESC + digit (alt+number)
    {complete, {key, [alt_key], <<C>>}};
try_parse_sequence(Seq) ->
    case parse_kitty_sequence(Seq) of
        {complete, _} = Result -> Result;
        _ -> check_incomplete_or_invalid(Seq)
    end.

parse_kitty_sequence([27, 91 | Rest]) ->
    case Rest of
        [] -> incomplete_or_invalid;
        _ ->
            case lists:last(Rest) of
                117 ->  % ends with 'u'
                    Params = lists:sublist(Rest, length(Rest) - 1),
                    case parse_kitty_params(Params) of
                        {Keycode, Modifiers} ->
                            Key = kitty_keycode_to_key(Keycode, Modifiers),
                            {complete, {key, Key}};
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

kitty_keycode_to_key(Keycode, 5) when Keycode >= 97, Keycode =< 122 ->
    [ctrl_key, <<(Keycode - 32)>>];  % Ctrl + letter
kitty_keycode_to_key(Keycode, _Mods) ->
    unicode:characters_to_binary([Keycode]).

check_incomplete_or_invalid([27]) -> {incomplete};
check_incomplete_or_invalid([27, 91]) -> {incomplete};
check_incomplete_or_invalid([27, 79]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 51]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 53]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 54]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 49]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 49, 53]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 49, 55]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 49, 56]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 49, 57]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 48]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 49]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 51]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 52]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 55]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 55, 59]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 55, 59, 53]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 55, 59, 53, 59]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 55, 59, 53, 59, 49]) -> {incomplete};
check_incomplete_or_invalid([27, 91, 50, 55, 59, 53, 59, 49, 51]) -> {incomplete};
check_incomplete_or_invalid([27, 91 | Rest]) ->
    case Rest of
        [] -> {incomplete};
        _ ->
            LastChar = lists:last(Rest),
            case LastChar of
                117 -> {incomplete};  % ends with 'u', might be incomplete kitty
                _ -> check_if_partial_kitty(Rest)
            end
    end;
check_incomplete_or_invalid(_) -> {invalid}.

check_if_partial_kitty(Rest) ->
    HasSemicolon = lists:any(fun(C) -> C =:= 59 end, Rest),
    HasDigit = lists:any(fun(C) -> C >= 48 andalso C =< 57 end, Rest),
    case HasSemicolon orelse HasDigit of
        true -> {incomplete};  % likely partial kitty sequence
        false -> {invalid}
    end.


lookup_color(Color, Type, default) when is_list(Color) ->
    erlang:error("NOT IMPLEMENTED");

lookup_color(Color, Type, normal) when is_list(Color) ->
    erlang:error("NOT IMPLEMENTED");

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
