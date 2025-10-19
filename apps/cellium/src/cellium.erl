-module(cellium).

-include("cellium.hrl").

%% API
-export([init/1, init/2, handle_call/3, handle_cast/2, render_caller/2]).
-export([start/1, handle_event/1]).

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
    logger:info("SOME BEHAVIOUR HANDLING CALL"),
    #{module := Module, model := Model} = State,

    % update return the model.
    NewModel = Module:update(Model, Msg),

    render_immediately(Module, NewModel),
    NewState = State#{model := NewModel},
    {reply, {ok, done}, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_event(Event) ->
    logger:info("some behavior - handle_event/1 param: ~p~n", [Event]),
    gen_server:call(cellium_server, Event).


render_immediately(Module, Model) ->
    Layout = Module:render(Model),
    NewLayout = layout:calculate_layout(Layout),
    view:set_root_widget(NewLayout),
    ok.

terminate(_Reason, _State) ->
    ?TERMBOX:tb_shutdown(),
    ok.
