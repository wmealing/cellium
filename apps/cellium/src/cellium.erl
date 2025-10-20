-module(cellium).

-include("cellium.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, render_caller/2]).
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

init(#{module := Module}= Args) ->
    logging:setup(),
    ?TERMBOX:tb_init(),
    view:start_link(),

    AutoFocus = maps:get(auto_widget_focus, Args, true),

    focus_manager:start_link(),
    cellium_event_manager:start_link(?MODULE),

    {ok, Model} = Module:init([]),

    render_immediately(Module, Model),

    State = #{module => Module,
              model => Model,
              auto_focus => AutoFocus },

    {ok, State}.

% Wraps update in the callback module.
handle_call(Msg, _From, State) ->
    #{module := Module, model := Model} = State,

    process_focus_event(State, Msg),

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

%% Internal functions
keycodes({tb_event, key, _, {keydata, Code1, Code2}}) ->
    {Code1, Code2};
keycodes(_AnythingElse) ->
    %% probably not a focus event
    ignore.

process_focus_event(#{auto_focus := true}, Event) ->
   case keycodes(Event) of
       %% tab
       {9,0} ->
           focus_manager:move_focus_forward();
       %% shift tab
       {65513,0} ->
           focus_manager:move_focus_backward();
       _Other ->
           ok
   end;

process_focus_event(_State, _Event) ->
    ignore.
