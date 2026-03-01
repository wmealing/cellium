-module(termbox_mock).
-moduledoc """
  A mock terminal implementation that stores the screen state in an ETS table.
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
-export([get_screen/0, get_cell/2, dump_screen/0]).

-record(state, {
    width = 80,
    height = 24,
    table_id
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
    Tid = ets:new(mock_terminal_buffer, [set, protected]),
    {ok, #state{table_id = Tid}}.

handle_call(tb_init, _From, State) ->
    ets:delete_all_objects(State#state.table_id),
    {reply, ok, State};

handle_call(tb_shutdown, _From, State) ->
    {reply, ok, State};

handle_call(tb_width, _From, State) ->
    {reply, State#state.width, State};

handle_call(tb_height, _From, State) ->
    {reply, State#state.height, State};

handle_call(tb_clear, _From, State) ->
    ets:delete_all_objects(State#state.table_id),
    {reply, ok, State};

handle_call({tb_set_cell, X, Y, Char, Fg, Bg}, _From, State) ->
    C = if is_binary(Char) -> hd(unicode:characters_to_list(Char));
           is_integer(Char) -> Char;
           true -> Char
        end,
    ets:insert(State#state.table_id, {{X, Y}, C, Fg, Bg}),
    {reply, ok, State};

handle_call({tb_print, X, Y, Fg, Bg, Str}, _From, State) ->
    Chars = if is_binary(Str) -> unicode:characters_to_list(Str);
               is_list(Str) -> Str;
               true -> []
            end,
    lists:foldl(fun(C, AccX) ->
        ets:insert(State#state.table_id, {{AccX, Y}, C, Fg, Bg}),
        AccX + 1
    end, X, Chars),
    {reply, ok, State};

handle_call(get_screen, _From, State) ->
    {reply, ets:tab2list(State#state.table_id), State};

handle_call({get_cell, X, Y}, _From, State) ->
    Result = case ets:lookup(State#state.table_id, {X, Y}) of
        [{_, Char, Fg, Bg}] -> {Char, Fg, Bg};
        [] -> {$\s, default, default}
    end,
    {reply, Result, State};

handle_call(dump_screen, _From, State) ->
    Dump = [ [ begin
                 {C, _, _} = case ets:lookup(State#state.table_id, {X, Y}) of
                                 [{_, Char, _, _}] -> {Char, 0, 0};
                                 [] -> {$\s, 0, 0}
                             end,
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
