-module(cellium_event_manager).

-moduledoc """
Event manager for handling terminal and external events.
""".

-behaviour(gen_server).


%% API
-export([start_link/0, start_link/1, send_event/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cellium.hrl").
-import(focus_manager, [move_focus_forward/0, move_focus_backward/0]).

-define(SERVER, ?MODULE).
-define(TICK_INTERVAL, 1). % ms

-record(state, {event_target=[]}).
%%%===================================================================
%%% API
%%%===================================================================

-doc "Starts the server.".
-spec start_link() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(CallBackModule) ->
    gen_server:start_link({local, ?SERVER}, 
                          ?MODULE, CallBackModule, []).

send_event(Event) ->
    gen_server:cast(?SERVER, {external_event, Event}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%% @private
-doc "Initializes the server.".
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.

init([]) ->
    init(foo);

init(Target) ->
    logger:info("Cellium event manager - init/1"),
    process_flag(trap_exit, true),
    
    %% Start a dedicated poller process that blocks on ?TERMINAL:term_poll_event()
    %% This keeps the gen_server responsive to cast/call while we wait for input.
    Self = self(),
    spawn_link(fun() -> event_poller_loop(Self) end),
    
    NewState = #state{event_target = Target},
    {ok, NewState}.

%% --- Poller Process ---

event_poller_loop(Parent) ->
    case ?TERMINAL:term_poll_event() of
        {error, _} = Error ->
            %% If the backend doesn't support polling (like mock), don't crash
            logger:debug("Terminal poll error: ~p", [Error]),
            timer:sleep(100),
            event_poller_loop(Parent);
        Event ->
            gen_server:cast(Parent, {terminal_event, Event}),
            event_poller_loop(Parent)
    end.

%%% @private
-doc "Handling call messages.".
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.

handle_call(Msg, _From, State) ->
    logger:info("Weird msg: ~p", [Msg]),
    {reply, ok, State}.

%%% @private
-doc "Handling cast messages.".
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.

handle_cast({terminal_event, Event}, State) ->
    process_event(Event, State),
    {noreply, State};

handle_cast({external_event, Event}, State) ->
    cellium:handle_event(Event),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%% @private
-doc "Handling all non call/cast messages.".
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(_Info, State) ->
    {noreply, State}.

%% --- Internal ---

process_event(Event, _State) ->
    case Event of
        {resize, W, H} ->
            logger:info("RESIZE EVENT DETECTED: ~p x ~p", [W, H]);
        {key, false, false, false, false, tab_key} ->
            focus_manager:move_focus_forward();
        {key, true, false, false, false, tab_key} ->
            focus_manager:move_focus_backward();
        {key, false, false, true, false, <<"l">>} -> % Ctrl-L
            logger:info("Ctrl-L detected, forcing redraw"),
            ?TERMINAL:term_force_redraw(),
            view:update_now();
        _ ->
            ok
    end,
    cellium:handle_event(Event).

%%% @private
-doc """
This function is called by a gen_server when it is about to
terminate. It should be the opposite of Module:init/1 and do any
necessary cleaning up. When it returns, the gen_server terminates
with Reason. The return value is ignored.
""".
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%% @private
-doc "Convert process state when code is changed.".
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


