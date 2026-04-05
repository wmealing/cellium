-module(terminal_dummy).
-moduledoc """
""".

-export([
    term_init/0,
    term_shutdown/0,
    term_width/0,
    term_height/0,
    term_clear/0,
    term_present/0,
    term_set_cell/5,
    term_print/5,
    get_cell/2,
    term_get_row/2,
    term_get_row/3,
    term_poll_event/0,
    get_next_event/0,
    term_set_input_mode/1,
    term_set_output_mode/1,
    term_force_redraw/0,
    get_buffer/0  % Testing helper
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
    buffer = #{} :: map()
}).



-define(SERVER, ?MODULE).

%% ===================================================================
%% Public API
%% ===================================================================

-doc "Initializes the terminal. Enables the alternate screen buffer and hides the cursor.".
term_init() ->
    case whereis(?SERVER) of
        undefined ->
            {ok, _} = start_link();
        _ ->
            ok
    end,
    gen_server:call(?SERVER, init_term).

-doc "Shuts down the terminal. Resets attributes, shows the cursor, and disables the alternate screen buffer.".
term_shutdown() ->
    gen_server:call(?SERVER, shutdown_term).

-doc "Returns the width of the terminal in columns.".
term_width() ->
    gen_server:call(?SERVER, get_width).

-doc "Returns the height of the terminal in rows.".
term_height() ->
    gen_server:call(?SERVER, get_height).

-doc "Clears the entire terminal screen.".
term_clear() ->
    gen_server:call(?SERVER, term_clear).

-doc "No-op for now, but would typically flush pending output to the terminal.".
term_present() ->
    ok.

-doc "Sets a single character at the specified (X, Y) position with given foreground and background colors.".
term_set_cell(X, Y, Char, Fg, Bg) ->
    gen_server:call(?SERVER, {term_set_cell, X, Y, Char, Fg, Bg}).


-doc "Prints a string at the specified (X, Y) position with given foreground and background colors.".
term_print(X, Y, Fg, Bg, Str) ->
    gen_server:call(?SERVER, {term_print, X, Y, Fg, Bg, Str}).

-doc "Returns the character and colors at the specified (X, Y) position from the internal shadow buffer.".
get_cell(X, Y) ->
    gen_server:call(?SERVER, {get_cell, X, Y}).

-doc "Returns an entire row of cells.".
term_get_row(Y, Width) ->
    gen_server:call(?SERVER, {get_row, Y, Width}).

-doc "Returns a segment of a row of cells.".
term_get_row(X, Y, Length) ->
    gen_server:call(?SERVER, {get_row, X, Y, Length}).

-doc "Polls for a terminal event. This function blocks until an event occurs.".
term_poll_event() ->
    gen_server:call(?SERVER, get_event, infinity).

-doc "Alias for term_poll_event/0. Polls for a terminal event.".
get_next_event() ->
    gen_server:call(?SERVER, get_event, infinity).

-doc "Sets the input mode for the terminal. Mode can be 'default' or 'alt'.".
term_set_input_mode(Mode) ->
    gen_server:call(?SERVER, {set_input_mode, Mode}).

-doc "Sets the output mode for the terminal. Mode can be 'default', 2 (256-color) 4, or 5 (truecolor).".
term_set_output_mode(Mode) ->
    gen_server:call(?SERVER, {set_output_mode, Mode}).

-doc "Forces a full redraw on the next term_present call.".
term_force_redraw() ->
    gen_server:call(?SERVER, term_force_redraw).

-doc "Returns the current buffer state for testing purposes.".
get_buffer() ->
    gen_server:call(?SERVER, get_buffer).

%% ===================================================================
%% Server implementation
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

init([]) ->
    {ok, #state{width=0, height=0, input_mode=alt, output_mode=default, buffer = cellium_buffer:empty()}}.

handle_call(init_term, _From, State) ->
    W = 100,
    H = 10, 
    {reply, ok, State#state{width=W, height=H, buffer = cellium_buffer:empty()}};

handle_call(shutdown_term, _From, State) ->
    {reply, ok, State};

handle_call(term_clear, _From, State) ->
    {reply, ok, State#state{buffer = cellium_buffer:empty()}};

handle_call({term_set_cell, X, Y, Char, Fg, Bg}, _From, State) ->
    OutputMode = State#state.output_mode,
    _FgAnsi = lookup_color(Fg, fg, OutputMode),
    _BgAnsi = lookup_color(Bg, bg, OutputMode),
    NewBuffer = cellium_buffer:set_cell(X, Y, Char, Fg, Bg, State#state.buffer),
    {reply, ok, State#state{buffer = NewBuffer}};

handle_call({term_print, X, Y, Fg, Bg, Str}, _From, State) ->
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, Str, State#state.buffer),
    {reply, ok, State#state{buffer = NewBuffer}};

handle_call({get_cell, X, Y}, _From, State) ->
    {reply, cellium_buffer:get_cell(X, Y, State#state.buffer), State};

handle_call({get_row, Y, Width}, _From, State) ->
    {reply, cellium_buffer:get_row(Y, Width, State#state.buffer), State};

handle_call({get_row, X, Y, Length}, _From, State) ->
    {reply, cellium_buffer:get_row(X, Y, Length, State#state.buffer), State};

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

    logger:info("Setting output mode: ~p", [OutputMode]),
    {reply, ok, State#state{output_mode = OutputMode}};

handle_call(get_output_mode, _From, State) ->
    {reply, State#state.output_mode, State};

handle_call(term_force_redraw, _From, State) ->
    {reply, ok, State};

handle_call(get_buffer, _From, State) ->
    {reply, State#state.buffer, State};

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

handle_cast({raw_char, _Char}, State) ->
    {noreply, State};

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
