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
    try
        Layout = Module:render(Model),
        view:set_root_widget(Layout)
    catch
        Exception:Reason:Stacktrace ->
            logger:error("Error in render: ~p:~p~n~p", [Exception, Reason, Stacktrace]),
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init(#{module := Module}= Args) ->
    view:start_link(),

    AutoFocus = maps:get(auto_focus, Args, true),
    ReportMouse = maps:get(report_mouse, Args, false),
    ColorType = maps:get(color_type, Args, output_normal),



    cellium_event_manager:start_link(?MODULE),

    case AutoFocus of
        true ->  focus_manager:start_link();
        _ -> ok
    end,

    case ReportMouse of
        true ->
            logger:debug("ENABLED MOUSE REPORTING"),
            ?TERMBOX:tb_set_input_mode(4);
        _ ->
            logger:debug("NO MOUSE REPORTING")
    end,

    case ColorType of
        none ->
            ?TERMBOX:tb_set_output_mode(4);
        truecolor ->
            ?TERMBOX:tb_set_output_mode(5);
        256  ->
            ?TERMBOX:tb_set_output_mode(2);
	normal ->
          ?TERMBOX:tb_set_output_mode(4);
        _AnythingElse ->
            logger:debug("Wierd color format set")
    end,

    {ok, Model} = Module:init([]),

    render_immediately(Module, Model),

    State = #{module => Module,
              model => Model},

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

terminate(Reason, _State) ->
    logger:info("Terminating cellium with reason: ~p", [Reason]),
    ?TERMBOX:tb_shutdown(),

    % set this as on option in a future verrsion.
    init:stop(),
    ok.

%% Internal functions
keycodes({tb_event, key, _, {keydata, Code1, Code2}}) ->
    {Code1, Code2};
keycodes(_AnythingElse) ->
    %% probably not a focus event
    ignore.


process_focus_event(_State, Event) ->
   case keycodes(Event) of
       %% tab
       {9,0} ->
           focus_manager:move_focus_forward();
       %% shift tab
       {65513,0} ->
           focus_manager:move_focus_backward();
       _Other ->
           ok
   end.
