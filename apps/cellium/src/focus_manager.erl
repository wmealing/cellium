-module(focus_manager).


-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([register_widget/2, unregister_widget/1]).
-export([set_focused/1, get_focused/0]).
-export([move_focus_forward/0, move_focus_backward/0]).
-export([can_focus/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cellium.hrl").

-define(SERVER, ?MODULE).

%% Internal state record
%% focusable_widgets: list of {WidgetId, WidgetRef} tuples in tab order
%% current_focus: the widget id that currently has focus
-record(state, {
    focusable_widgets = [],
    current_focus = none
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc
%% Starts the focus manager gen_server.
%% Initializes with an empty list of focusable widgets.
%% @end
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Starts the focus manager with initial configuration.
%% Config can contain initial focusable widget list and starting focus.
%% @end
-spec start_link(Config :: map()) -> {ok, Pid :: pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc
%% Registers a widget as focusable in the application.
%% WidgetId is a unique identifier for the widget.
%% WidgetRef is a reference or handle to the actual widget.
%% Returns ok on success or {error, Reason} on failure.
%%
%% Widgets are registered in the order they can be focused (tab order).
%% @end
-spec register_widget(WidgetId :: term(), WidgetRef :: term()) ->
    ok | {error, already_registered | invalid_widget}.
register_widget(WidgetId, WidgetRef) ->
    gen_server:call(?SERVER, {register_widget, WidgetId, WidgetRef}).

%% @doc
%% Unregisters a widget from the focusable widget list.
%% If the unregistered widget had focus, focus moves to the next widget.
%% Returns ok on success or {error, not_found} if widget not registered.
%% @end
-spec unregister_widget(WidgetId :: term()) ->
    ok | {error, not_found}.
unregister_widget(WidgetId) ->
    gen_server:call(?SERVER, {unregister_widget, WidgetId}).

%% @doc
%% Sets focus to a specific widget by its ID.
%% Sends a focus_changed event with {lost, PreviousId} and {gained, NewId}.
%% Returns ok on success or {error, not_found} if widget doesn't exist.
%% @end
-spec set_focused(WidgetId :: term()) ->
    ok | {error, not_found}.
set_focused(WidgetId) ->
    gen_server:call(?SERVER, {set_focused, WidgetId}).

%% @doc
%% Returns the ID of the currently focused widget.
%% Returns {ok, WidgetId} or {error, no_focus} if no widget has focus.
%% @end
-spec get_focused() ->
    {ok, WidgetId :: term()} | {error, no_focus}.
get_focused() ->
    gen_server:call(?SERVER, get_focused).

%% @doc
%% Moves focus to the next focusable widget in tab order.
%% If at the end of the list, wraps around to the beginning.
%% Returns ok on success or {error, no_widgets} if no focusable widgets registered.
%% @end
-spec move_focus_forward() ->
    ok | {error, no_widgets}.
move_focus_forward() ->
    gen_server:call(?SERVER, move_focus_forward).

%% @doc
%% Moves focus to the previous focusable widget in reverse tab order.
%% If at the beginning of the list, wraps around to the end.
%% Returns ok on success or {error, no_widgets} if no focusable widgets registered.
%% @end
-spec move_focus_backward() ->
    ok | {error, no_widgets}.
move_focus_backward() ->
    gen_server:call(?SERVER, move_focus_backward).

%% @doc
%% Checks if a widget with the given ID is registered and focusable.
%% Returns true if the widget is focusable, false otherwise.
%% @end
-spec can_focus(WidgetId :: term()) ->
    true | false.
can_focus(WidgetId) ->
    gen_server:call(?SERVER, {can_focus, WidgetId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initializes the focus manager state.
%% Sets up the empty focusable widget list and clears any initial focus.
%% @end
-spec init(Config :: map() | []) ->
    {ok, State :: #state{}}.
init(_Config) ->
    {ok, #state{focusable_widgets = [], current_focus = none}}.

%% @private
%% @doc
%% Handles synchronous calls from clients.
%% Delegates to specific handler functions for each operation.
%% @end
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}}.

handle_call({register_widget, WidgetId, WidgetRef}, _From, State) ->
    handle_register_widget(WidgetId, WidgetRef, State);

handle_call({unregister_widget, WidgetId}, _From, State) ->
    handle_unregister_widget(WidgetId, State);

handle_call({set_focused, WidgetId}, _From, State) ->
    handle_set_focused(WidgetId, State);

handle_call(get_focused, _From, State) ->
    Reply = {ok, State#state.current_focus},
    {reply, Reply, State};

handle_call(move_focus_forward, _From, State) ->
    handle_move_focus_forward(State);

handle_call(move_focus_backward, _From, State) ->
    handle_move_focus_backward(State);

handle_call({can_focus, WidgetId}, _From, State) ->
    Reply = is_focusable(WidgetId, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason :: term(), State :: #state{}) ->
    ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) ->
    {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal handler functions
%%%===================================================================

%% @doc
%% Handles widget registration.
%% Appends widget to the focusable widgets list if not already present.
%% If this is the first widget, automatically sets it as focused.
%% @end
-spec handle_register_widget(WidgetId :: term(), WidgetRef :: term(), 
                             State :: #state{}) ->
    {reply, Reply :: ok | {error, term()}, NewState :: #state{}}.
handle_register_widget(WidgetId, _WidgetRef, #state{focusable_widgets = Widgets} = State) ->
    case is_focusable(WidgetId, State) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            NewWidgets = Widgets ++ [WidgetId],
            NewFocus = case State#state.current_focus of
                none -> WidgetId;
                _ -> State#state.current_focus
            end,
            NewState = State#state{
                focusable_widgets = NewWidgets,
                current_focus = NewFocus
            },
            {reply, ok, NewState}
    end.

%% @doc
%% Handles widget unregistration.
%% Removes widget from focusable list.
%% If the removed widget had focus, moves focus to next widget or none.
%% @end
-spec handle_unregister_widget(WidgetId :: term(), State :: #state{}) ->
    {reply, Reply :: ok | {error, not_found}, NewState :: #state{}}.
handle_unregister_widget(WidgetId, #state{focusable_widgets = Widgets, 
                                          current_focus = CurrentFocus} = State) ->
    case is_focusable(WidgetId, State) of
        false ->
            {reply, {error, not_found}, State};
        true ->
            NewWidgets = lists:delete(WidgetId, Widgets),
            NewFocus = case CurrentFocus of
                WidgetId -> 
                    case NewWidgets of
                        [] -> none;
                        _ -> hd(NewWidgets)
                    end;
                _ -> CurrentFocus
            end,
            NewState = State#state{
                focusable_widgets = NewWidgets,
                current_focus = NewFocus
            },
            {reply, ok, NewState}
    end.

%% @doc
%% Handles setting focus to a specific widget.
%% Emits focus change events and updates internal state.
%% @end
-spec handle_set_focused(WidgetId :: term(), State :: #state{}) ->
    {reply, Reply :: ok | {error, not_found}, NewState :: #state{}}.
handle_set_focused(WidgetId, State) ->
    case is_focusable(WidgetId, State) of
        false ->
            {reply, {error, not_found}, State};
        true ->
            OldFocus = State#state.current_focus,
            emit_focus_changed(OldFocus, WidgetId),
            NewState = State#state{current_focus = WidgetId},
            {reply, ok, NewState}
    end.

%% @doc
%% Handles moving focus forward to the next widget.
%% Wraps around from last to first widget.
%% @end
-spec handle_move_focus_forward(State :: #state{}) ->
    {reply, Reply :: ok | {error, no_widgets}, NewState :: #state{}}.
handle_move_focus_forward(#state{focusable_widgets = []} = State) ->
    {reply, {error, no_widgets}, State};
handle_move_focus_forward(#state{focusable_widgets = Widgets, 
                                 current_focus = CurrentFocus} = State) ->
    NextId = get_next_focus(CurrentFocus, Widgets),
    emit_focus_changed(CurrentFocus, NextId),
    NewState = State#state{current_focus = NextId},
    {reply, ok, NewState}.

%% @doc
%% Handles moving focus backward to the previous widget.
%% Wraps around from first to last widget.
%% @end
-spec handle_move_focus_backward(State :: #state{}) ->
    {reply, Reply :: ok | {error, no_widgets}, NewState :: #state{}}.
handle_move_focus_backward(#state{focusable_widgets = []} = State) ->
    {reply, {error, no_widgets}, State};
handle_move_focus_backward(#state{focusable_widgets = Widgets, 
                                  current_focus = CurrentFocus} = State) ->
    PrevId = get_prev_focus(CurrentFocus, Widgets),
    emit_focus_changed(CurrentFocus, PrevId),
    NewState = State#state{current_focus = PrevId},
    {reply, ok, NewState}.

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @doc
%% Checks if a widget is focusable (in the registered list).
%% @end
-spec is_focusable(WidgetId :: term(), State :: #state{}) ->
    true | false.
is_focusable(WidgetId, #state{focusable_widgets = Widgets}) ->
    lists:member(WidgetId, Widgets).

%% @doc
%% Gets the next widget in focus order, wrapping around if needed.
%% If current focus is none, returns the first widget.
%% @end
-spec get_next_focus(CurrentId :: term(), Widgets :: [term()]) ->
    NextId :: term().
get_next_focus(none, Widgets) ->
    hd(Widgets);
get_next_focus(CurrentId, Widgets) ->
    case get_focus_index(CurrentId, Widgets) of
        not_found -> 
            hd(Widgets);
        Index ->
            case Index + 1 > length(Widgets) of
                true -> hd(Widgets);
                false -> lists:nth(Index + 1, Widgets)
            end
    end.

%% @doc
%% Gets the previous widget in focus order, wrapping around if needed.
%% If current focus is none, returns the last widget.
%% @end
-spec get_prev_focus(CurrentId :: term(), Widgets :: [term()]) ->
    PrevId :: term().
get_prev_focus(none, Widgets) ->
    lists:last(Widgets);
get_prev_focus(CurrentId, Widgets) ->
    case get_focus_index(CurrentId, Widgets) of
        not_found -> 
            lists:last(Widgets);
        Index ->
            case Index - 1 < 1 of
                true -> lists:last(Widgets);
                false -> lists:nth(Index - 1, Widgets)
            end
    end.

%% @doc
%% Gets the 1-based index of a widget in the focus list.
%% Returns not_found if widget not in list.
%% @end
-spec get_focus_index(WidgetId :: term(), Widgets :: [term()]) ->
    Index :: integer() | not_found.
get_focus_index(WidgetId, Widgets) ->
    find_index(WidgetId, Widgets, 1).

find_index(_Item, [], _Index) ->
    not_found;
find_index(Item, [Item | _Rest], Index) ->
    Index;
find_index(Item, [_ | Rest], Index) ->
    find_index(Item, Rest, Index + 1).

%% @doc
%% Emits a focus_changed event to notify other parts of the system.
%% This allows widgets to update their visual indicators.
%% Should integrate with cellium_event_manager or similar.
%% @end
-spec emit_focus_changed(OldId :: term(), NewId :: term()) ->
    ok.
emit_focus_changed(OldId, NewId) ->
    logger:info("Focus changed from ~p to ~p~n", [OldId, NewId]),
    %% TODO: Send event to cellium_event_manager or similar
    %% Event format: {focus_changed, #{from => OldId, to => NewId}}
    ok.

