-module(screen).
-moduledoc """
Screen management module for handling UI screen lifecycle and transitions.

This module provides a higher-level abstraction over widgets and focus management,
allowing applications to manage distinct screens (like search, customer form, etc.)
with automatic cleanup and focus handling.

## Screen Lifecycle

1. **Create**: Build a screen's widget tree using DSL or direct construction
2. **Show**: Display the screen and register focusable widgets
3. **Hide**: Hide the screen and unregister widgets (without destroying)
4. **Destroy**: Permanently remove the screen and clean up resources

## Usage

Basic screen transition:
```
% Build the new screen
CustomerScreen = screen:new(customer_form, fun() ->
    cellium_dsl:from_dsl({vbox, [{id, customer_form}], [
        {text_input, [{id, name_field}, {focusable, true}]},
        {button, [{id, save_btn}, {focusable, true}], "Save"}
    ]})
end),

% Switch from search to customer screen
screen:transition(SearchScreen, CustomerScreen)
```

Using a screen stack:
```
% Push a new screen (like opening a dialog)
screen:push(DialogScreen),

% Pop back to previous screen
screen:pop()
```

## Screen Structure

A screen is a map containing:
- `id`: Unique screen identifier
- `widget_tree`: The root widget (typically a container)
- `active`: Whether the screen is currently shown
- `builder`: Optional function to rebuild the screen
""".

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([new/2, new/3]).
-export([show/1, hide/1, destroy/1]).
-export([transition/2, replace/1]).
-export([push/1, pop/0, current/0, stack/0]).
-export([get_widget_tree/1, update_widget_tree/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Screen stack state
-record(state, {
    stack = [],      % List of screen maps, head is current
    history = []     % List of previous screen IDs for debugging
}).

%%%===================================================================
%%% API
%%%===================================================================

-doc """
Starts the screen manager gen_server.

The screen manager maintains a stack of screens and handles transitions
between them. It is automatically started by the cellium supervisor.
""".
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc """
Creates a new screen with a widget tree.

The widget tree should already be built but not yet "created" (widgets not registered).
Call `screen:show/1` to activate the screen and register its widgets.

## Example

```
SearchScreen = screen:new(search_screen,
    cellium_dsl:from_dsl({vbox, [], [
        {text_input, [{id, search_box}, {focusable, true}]}
    ]}))
```
""".
-spec new(ScreenId :: term(), WidgetTree :: map()) -> map().
new(ScreenId, WidgetTree) ->
    #{
        id => ScreenId,
        widget_tree => WidgetTree,
        active => false,
        builder => undefined
    }.

-doc """
Creates a new screen with a builder function.

The builder function will be called to create the widget tree when the screen is shown.
This is useful for screens that need to rebuild with fresh data each time they're displayed.

## Example

```
screen:new(customer_form,
    fun() -> build_customer_form(get_customer_data()) end,
    empty_widget())
```
""".
-spec new(ScreenId :: term(), BuilderFun :: fun(() -> map()), InitialTree :: map()) -> map().
new(ScreenId, BuilderFun, InitialTree) ->
    #{
        id => ScreenId,
        widget_tree => InitialTree,
        active => false,
        builder => BuilderFun
    }.

-doc """
Shows a screen, registering all its focusable widgets with the focus manager.

If the screen has a builder function, it will be called to rebuild the widget tree.
Returns the screen with `active => true`.
""".
-spec show(Screen :: map()) -> map().
show(#{builder := Builder, id := Id} = Screen) when is_function(Builder) ->
    logger:info("Showing screen ~p (rebuilding)", [Id]),
    NewTree = Builder(),
    register_widget_tree(NewTree),
    Screen#{widget_tree => NewTree, active => true};
show(#{widget_tree := Tree, id := Id} = Screen) ->
    logger:info("Showing screen ~p", [Id]),
    register_widget_tree(Tree),
    Screen#{active => true}.

-doc """
Hides a screen, unregistering all its focusable widgets from the focus manager.

The widget tree is preserved so the screen can be shown again later.
Returns the screen with `active => false`.
""".
-spec hide(Screen :: map()) -> map().
hide(#{widget_tree := Tree, id := Id} = Screen) ->
    logger:info("Hiding screen ~p", [Id]),
    widget:destroy_tree(Tree),
    Screen#{active => false}.

-doc """
Destroys a screen permanently, cleaning up all widgets.

Use this when a screen will not be used again. For temporary hiding,
use `screen:hide/1` instead.
""".
-spec destroy(Screen :: map()) -> ok.
destroy(#{widget_tree := Tree, id := Id}) ->
    logger:info("Destroying screen ~p", [Id]),
    widget:destroy_tree(Tree),
    ok.

-doc """
Transitions from one screen to another.

Hides the old screen (unregistering its widgets) and shows the new screen
(registering its widgets). This is the recommended way to switch between screens.

Returns the new screen with `active => true`.

## Example

```
NewScreen = screen:transition(SearchScreen, CustomerFormScreen)
```
""".
-spec transition(OldScreen :: map(), NewScreen :: map()) -> map().
transition(OldScreen, NewScreen) ->
    hide(OldScreen),
    show(NewScreen).

-doc """
Replaces the current screen in the managed stack with a new screen.

If there is a current screen, it is hidden and the new screen is shown.
The current screen is replaced in the stack (not pushed).

Requires the screen manager to be running.
""".
-spec replace(NewScreen :: map()) -> ok | {error, term()}.
replace(NewScreen) ->
    gen_server:call(?SERVER, {replace, NewScreen}).

-doc """
Pushes a new screen onto the stack, hiding the current screen.

The current screen remains in memory and can be returned to with `screen:pop/0`.
This is useful for modal dialogs or nested navigation.

## Example

```
screen:push(ConfirmDialog),  % Shows dialog, hides current screen
% User interacts with dialog...
screen:pop()                  % Returns to previous screen
```
""".
-spec push(NewScreen :: map()) -> ok | {error, term()}.
push(NewScreen) ->
    gen_server:call(?SERVER, {push, NewScreen}).

-doc """
Pops the current screen from the stack, returning to the previous screen.

The popped screen is destroyed. The previous screen is shown and becomes current.
Returns `{ok, PreviousScreenId}` or `{error, no_previous_screen}` if there is
only one screen in the stack.
""".
-spec pop() -> {ok, ScreenId :: term()} | {error, no_previous_screen}.
pop() ->
    gen_server:call(?SERVER, pop).

-doc """
Returns the current active screen, if any.

Returns `{ok, Screen}` or `{error, no_screen}` if the stack is empty.
""".
-spec current() -> {ok, map()} | {error, no_screen}.
current() ->
    gen_server:call(?SERVER, current).

-doc """
Returns the entire screen stack for debugging purposes.

The list is ordered with the current screen first, followed by previously
pushed screens.
""".
-spec stack() -> [map()].
stack() ->
    gen_server:call(?SERVER, stack).

-doc """
Gets the widget tree from a screen.

Returns the root widget map of the screen's widget tree.
""".
-spec get_widget_tree(Screen :: map()) -> map().
get_widget_tree(#{widget_tree := Tree}) ->
    Tree.

-doc """
Updates the widget tree of a screen.

If the screen is active, destroys the old tree and registers the new one.
If the screen is inactive, just updates the tree without registration.

Returns the updated screen map.
""".
-spec update_widget_tree(Screen :: map(), NewTree :: map()) -> map().
update_widget_tree(#{active := true, widget_tree := OldTree} = Screen, NewTree) ->
    widget:destroy_tree(OldTree),
    register_widget_tree(NewTree),
    Screen#{widget_tree => NewTree};
update_widget_tree(Screen, NewTree) ->
    Screen#{widget_tree => NewTree}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    logger:info("Starting screen manager"),
    {ok, #state{stack = [], history = []}}.

%% @private
handle_call({replace, NewScreen}, _From, State) ->
    case State#state.stack of
        [] ->
            % No current screen, just show the new one
            ShownScreen = show(NewScreen),
            NewState = State#state{
                stack = [ShownScreen],
                history = [maps:get(id, ShownScreen) | State#state.history]
            },
            {reply, ok, NewState};
        [CurrentScreen | Rest] ->
            % Hide current, show new
            hide(CurrentScreen),
            ShownScreen = show(NewScreen),
            NewState = State#state{
                stack = [ShownScreen | Rest],
                history = [maps:get(id, ShownScreen) | State#state.history]
            },
            {reply, ok, NewState}
    end;

handle_call({push, NewScreen}, _From, State) ->
    case State#state.stack of
        [] ->
            % No current screen, just show the new one
            ShownScreen = show(NewScreen),
            NewState = State#state{
                stack = [ShownScreen],
                history = [maps:get(id, ShownScreen) | State#state.history]
            },
            {reply, ok, NewState};
        [CurrentScreen | _] = Stack ->
            % Hide current (don't destroy), show new
            HiddenCurrent = hide(CurrentScreen),
            ShownScreen = show(NewScreen),
            NewState = State#state{
                stack = [ShownScreen, HiddenCurrent | tl(Stack)],
                history = [maps:get(id, ShownScreen) | State#state.history]
            },
            {reply, ok, NewState}
    end;

handle_call(pop, _From, State) ->
    case State#state.stack of
        [] ->
            {reply, {error, no_previous_screen}, State};
        [_Only] ->
            {reply, {error, no_previous_screen}, State};
        [CurrentScreen, PreviousScreen | Rest] ->
            % Destroy current, show previous
            destroy(CurrentScreen),
            ShownPrevious = show(PreviousScreen),
            NewState = State#state{
                stack = [ShownPrevious | Rest],
                history = [maps:get(id, ShownPrevious) | State#state.history]
            },
            {reply, {ok, maps:get(id, ShownPrevious)}, NewState}
    end;

handle_call(current, _From, State) ->
    case State#state.stack of
        [] -> {reply, {error, no_screen}, State};
        [Current | _] -> {reply, {ok, Current}, State}
    end;

handle_call(stack, _From, State) ->
    {reply, State#state.stack, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Recursively registers all widgets in a widget tree.
%% Calls widget:create/1 on each widget to register focusable ones.
%% @end
-spec register_widget_tree(WidgetTree :: map()) -> ok.
register_widget_tree(Widget) ->
    % Register children first if this is a container
    case maps:get(children, Widget, undefined) of
        undefined -> ok;
        Children when is_list(Children) ->
            lists:foreach(fun register_widget_tree/1, Children);
        _ -> ok
    end,
    % Then register this widget (this will register if focusable)
    widget:create(Widget),
    ok.
