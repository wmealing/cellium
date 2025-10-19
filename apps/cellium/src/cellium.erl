-module(cellium).

-include("cellium.hrl").

%% API
-export([init/1, init/2, handle_call/3, handle_cast/2, render_caller/2]).
-export([start/1, stop/0, handle_event/1, terminate/2]).

-behaviour(gen_server).

-callback init(Args :: term()) ->
     {ok, Args :: term()} | ignore.

-callback update(Args ::term(), Msg ::term() ) ->
    {ok, Things :: term() }.

-callback render(Model :: term() ) ->
    {ok, Layout :: term()} | {error , Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

start(Args) ->
    gen_server:start_link({local, cellium_server}, ?MODULE, Args, [] ).

stop() ->
    logging:info("STOP() called", []),
    gen_server:cast(?MODULE, stop).

render_caller(Module, Model) ->
    Layout = Module:render(Model),
    view:set_root_widget(Layout).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init(Module) ->
    logging:setup(),
    ?TERMBOX:tb_init(),
    init(Module, []).

init(Module, Args) ->
    view:start_link(),
    cellium_event_manager:start_link(?MODULE),

    {ok, Model} = Module:init(Args),

    render_immediately(Module, Model),
    State = #{module => Module, model => Model},
    {ok, State}.

% Wraps update in the callback module.
handle_call(Msg, _From, State) ->
    #{module := Module, model := Model} = State,

    % update return the model.
    NewModel = Module:update(Model, Msg),

    render_immediately(Module, NewModel),
    NewState = State#{model := NewModel},
    {reply, {ok, done}, NewState}.

handle_cast(stop, State) ->
    logging:info("CAST STOP - CELLIUM"),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_event(Event) ->
    gen_server:call(cellium_server, Event).

render_immediately(Module, Model) ->
    Layout = Module:render(Model),
    NewLayout = layout:calculate_layout(Layout),
    view:set_root_widget(NewLayout),
    ok.

terminate(_Reason, _State) ->

    ?TERMBOX:tb_shutdown(),
    logging:teardown(),

    % set this as on option in a future verrsion.
    init:stop(),
    ok.
