-module(cellium).
-moduledoc "This module defines the `cellium` behaviour, which is the core of a Cellium application.\n\nIt's a `gen_server` that manages the application's state, event handling, and rendering\nloop. To create an application, you must implement the callback functions defined in this\nbehaviour.\n\nThe three main callbacks are:\n- `c:init/1`: To set up the initial state of the application (the model).\n- `c:update/2`: To handle events and update the application model.\n- `c:render/1`: To define the UI by transforming the model into a widget tree.\n\nA simple application would look like this:\n```\n-module(my_app).\n-behaviour(cellium).\n\n-export([init/1, update/2, render/1]).\n\ninit(_Args) ->\n    {ok, #{text => <<\"Hello, World!\">>}}.\n\nupdate(Model, _Msg) ->\n    Model.\n\nrender(#{text := Text}) ->\n    text:new(my_text, Text).\n```".

-include("cellium.hrl").
-import(focus_manager, [remove_all/0]).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, render_caller/2]).
-export([start/1, stop/0, handle_event/1, terminate/2]).

-behaviour(gen_server).

-doc "Invoked when the application starts. It should initialize the application's state,\nreferred to as the 'model'.".
-callback init(Args :: term()) -> {ok, Model :: term()} | ignore.

-doc "Handles incoming messages and updates the application state.\n`Msg` is the message to process, and `Model` is the current state. It should return the new state `NewModel`.".
-callback update(Model :: term(), Msg :: term()) -> NewModel :: term().

-doc "Takes the current application `Model` and returns a widget tree (`Layout`) to be rendered on the screen.\nThis function defines the UI of the application.".
-callback render(Model :: term()) -> Layout :: term().

%%%===================================================================
%%% API
%%%===================================================================

-doc "Starts the Cellium application.\n`Args` is a map containing configuration, including the `module` that implements the `cellium` behaviour.".
-spec start(Args :: map()) -> {ok, pid()} | {error, any()}.
start(Args) ->
    application:ensure_all_started(cellium),
    gen_server:start_link({local, cellium_server}, ?MODULE, Args, []).

-doc "Stops the Cellium application gracefully.".
-spec stop() -> ok.
stop() ->
    logger:info("STOP() called"),
    gen_server:cast(cellium_server, stop).

render_caller(Module, Model) ->
    try
        % Reset focusable widgets list for this render, but preserve current focus ID
        focus_manager:remove_all(),

        Layout = Module:render(Model),
        ProcessedLayout = case is_tuple(Layout) of
            true -> cellium_dsl:from_dsl(Layout, Model);
            false -> Layout
        end,
        view:set_root_widget(ProcessedLayout),
        ok
    catch
        Exception:Reason:Stacktrace ->
            logger:error("Error in render: ~p:~p~nStacktrace: ~p", [Exception, Reason, Stacktrace]),
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

    %% NOTE: focus_manager is now started by cellium_sup
    case AutoFocus of
        true ->
            logger:info("Auto focus enabled (managed by supervisor)");
        _ ->
            logger:warning("Auto focus disabled - focus_manager still runs but widgets won't be registered")
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
            logger:debug("Weird color format set")
    end,

    {ok, Model} = Module:init([]),

    render_caller(Module, Model),

    State = #{module => Module,
              model => Model},

    {ok, State}.

% Wraps update in the callback module.
handle_call(Msg, _From, State) ->
    #{module := Module, model := Model} = State,

    % 1. Try to handle component events automatically
    Model1 = maybe_handle_component_event(Msg, Model),

    % 2. Flush any messages sent to self() (like button_clicked)
    % so they are processed in this same render cycle.
    Model2 = flush_emitted_messages(Model1, Module),

    % 3. Call user's update with (possibly) updated model
    FinalModel = Module:update(Model2, Msg),

    case Msg of
        {resize, _, _} ->
            % Also ensure the view is updated immediately for resize
            ?TERMBOX:tb_force_redraw();
        _ ->
            ok
    end,

    render_caller(Module, FinalModel),

    NewState = State#{model := FinalModel},
    {reply, {ok, done}, NewState}.

flush_emitted_messages(Model, Module) ->
    receive
        {button_clicked, _} = Msg ->
            flush_emitted_messages(Module:update(Model, Msg), Module);
        {radio_selected, _, _} = Msg ->
            flush_emitted_messages(Module:update(Model, Msg), Module);
        {checkbox_toggled, _, _} = Msg ->
            flush_emitted_messages(Module:update(Model, Msg), Module)
    after 0 ->
        Model
    end.

handle_cast(stop, State) ->
    logger:info("CAST STOP - CELLIUM"),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    #{module := Module, model := Model} = State,

    % 1. Try to handle component events automatically
    Model1 = maybe_handle_component_event(Msg, Model),

    % 2. Flush emitted messages
    Model2 = flush_emitted_messages(Model1, Module),

    % 3. Call user's update
    FinalModel = Module:update(Model2, Msg),

    render_caller(Module, FinalModel),
    {noreply, State#{model := FinalModel}}.

maybe_handle_component_event(Msg, Model) ->
    case focus_manager:get_focused() of
        {ok, none} -> Model;
        {ok, Id} ->
            case find_widget_by_id(Id, view:get_root_widget()) of
                undefined -> Model;
                Widget ->
                    Type = maps:get(widget_type, Widget, undefined),
                    case Type of
                        undefined -> Model;
                        _ ->
                            % Check if Type module has handle_event/2
                            try
                                Exports = Type:module_info(exports),
                                case lists:member({handle_event, 2}, Exports) of
                                    true ->
                                        States = maps:get(widget_states, Model, #{}),
                                        CurrentState = maps:get(Id, States, Widget),
                                        NewWidgetState = Type:handle_event(Msg, CurrentState),
                                        Model#{widget_states => States#{Id => NewWidgetState}};
                                    false -> Model
                                end
                            catch
                                _:_ -> Model
                            end
                    end
            end;
        _ -> Model
    end.

find_widget_by_id(Id, #{id := Id} = Widget) ->
    Widget;
find_widget_by_id(Id, #{children := Children}) when is_list(Children) ->
    lists:foldl(fun(Child, Acc) ->
        case Acc of
            undefined -> find_widget_by_id(Id, Child);
            _ -> Acc
        end
    end, undefined, Children);
find_widget_by_id(_Id, _Widget) ->
    undefined.

-doc "Sends an event to the application's `update/2` callback for processing.\nThis is the primary way to send messages to the running application.".
-spec handle_event(Event :: term()) -> {ok, done}.
handle_event(Event) ->
    gen_server:call(cellium_server, Event).

terminate(Reason, _State) ->
    logger:info("Terminating cellium with reason: ~p", [Reason]),
    ?TERMBOX:tb_shutdown(),

    % set this as on option in a future verrsion.
    init:stop(),
    focus_manager:remove_all(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
