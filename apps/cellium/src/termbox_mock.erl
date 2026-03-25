-module(termbox_mock).
-moduledoc """
  A mock terminal implementation that stores the screen state in a Map buffer.
  This allows for verifying what was drawn to the screen during tests.
""".

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Terminal API
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
    tb_set_input_mode/1,
    tb_set_output_mode/1
]).

%% Mock-specific API
-export([get_screen/0, get_cell/2, tb_get_row/2, tb_get_row/3, dump_screen/0]).

-record(state, {
    width = 80,
    height = 24,
    buffer = #{}
}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% Terminal API
%% ===================================================================

tb_init() ->
    case whereis(?SERVER) of
        undefined -> {ok, _} = start_link();
        _ -> ok
    end,
    gen_server:call(?SERVER, tb_init).

tb_shutdown() ->
    gen_server:call(?SERVER, tb_shutdown).

tb_width() ->
    gen_server:call(?SERVER, tb_width).

tb_height() ->
    gen_server:call(?SERVER, tb_height).

tb_clear() ->
    gen_server:call(?SERVER, tb_clear).

tb_present() ->
    ok.

tb_set_cell(X, Y, Char, Fg, Bg) ->
    gen_server:call(?SERVER, {tb_set_cell, X, Y, Char, Fg, Bg}).

tb_print(X, Y, Fg, Bg, Str) ->
    gen_server:call(?SERVER, {tb_print, X, Y, Fg, Bg, Str}).

tb_poll_event() ->
    {error, not_supported_in_mock}.

tb_set_input_mode(_) -> ok.
tb_set_output_mode(_) -> ok.

%% ===================================================================
%% Mock API
%% ===================================================================

get_screen() ->
    gen_server:call(?SERVER, get_screen).

get_cell(X, Y) ->
    gen_server:call(?SERVER, {get_cell, X, Y}).

tb_get_row(Y, Width) ->
    gen_server:call(?SERVER, {get_row, Y, Width}).

tb_get_row(X, Y, Length) ->
    gen_server:call(?SERVER, {get_row, X, Y, Length}).

dump_screen() ->
    gen_server:call(?SERVER, dump_screen).

%% ===================================================================
%% GenServer Implementation
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

init([]) ->
    {ok, #state{buffer = cellium_buffer:empty()}}.

handle_call(tb_init, _From, State) ->
    {reply, ok, State#state{buffer = cellium_buffer:empty()}};

handle_call(tb_shutdown, _From, State) ->
    {reply, ok, State};

handle_call(tb_width, _From, State) ->
    {reply, State#state.width, State};

handle_call(tb_height, _From, State) ->
    {reply, State#state.height, State};

handle_call(tb_clear, _From, State) ->
    {reply, ok, State#state{buffer = cellium_buffer:empty()}};

handle_call({tb_set_cell, X, Y, Char, Fg, Bg}, _From, State) ->
    NewBuffer = cellium_buffer:set_cell(X, Y, Char, Fg, Bg, State#state.buffer),
    {reply, ok, State#state{buffer = NewBuffer}};

handle_call({tb_print, X, Y, Fg, Bg, Str}, _From, State) ->
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, Str, State#state.buffer),
    {reply, ok, State#state{buffer = NewBuffer}};

handle_call(get_screen, _From, State) ->
    %% Convert map to list matching ets:tab2list style for compatibility if needed
    List = [ {{X, Y}, C, F, B} || {{X, Y}, {C, F, B}} <- maps:to_list(State#state.buffer) ],
    {reply, List, State};

handle_call({get_cell, X, Y}, _From, State) ->
    {reply, cellium_buffer:get_cell(X, Y, State#state.buffer), State};

handle_call({get_row, Y, Width}, _From, State) ->
    {reply, cellium_buffer:get_row(Y, Width, State#state.buffer), State};

handle_call({get_row, X, Y, Length}, _From, State) ->
    {reply, cellium_buffer:get_row(X, Y, Length, State#state.buffer), State};

handle_call(dump_screen, _From, State) ->
    Dump = [ [ begin
                 {C, _, _} = cellium_buffer:get_cell(X, Y, State#state.buffer),
                 C
               end || X <- lists:seq(0, State#state.width - 1) ]
             || Y <- lists:seq(0, State#state.height - 1) ],
    {reply, Dump, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
