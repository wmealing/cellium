-module(view).
-moduledoc """
View server responsible for rendering the widget tree to the terminal.

This gen_server manages the rendering lifecycle, including:
- Loading and applying CSS stylesheets
- Calculating layouts from widget trees
- Dirty checking to avoid unnecessary redraws
- Handling terminal resizes
- Managing the double-buffered rendering system

The view uses a dirty-checking optimization where it only re-renders when:
- The root widget tree has changed
- The terminal has been resized
- A forced redraw is requested

## Rendering Pipeline

1. Widget tree → Layout calculation → CSS styling → Buffer rendering → Terminal output

## Double Buffering

The underlying terminal backend maintains front and back buffers,
only sending diff updates to the terminal for changed cells.
""".

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).
-export([set_root_widget/1, get_root_widget/0, update_now/0]).

-include("cellium.hrl").

-record(state, {
    root_widget = #{},
    last_root_widget = undefined,
    width = 0,
    height = 0,
    stylesheet = #{} ,
    force_redraw = false
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    _Initiated = ?TERMBOX:tb_init(),
    _Cleared = ?TERMBOX:tb_clear(),
    _Presented = ?TERMBOX:tb_present(),

    W = ?TERMBOX:tb_width(),
    H = ?TERMBOX:tb_height(),

    % Load stylesheet once at startup
    StylesheetPath = "priv/default_theme.css",
    Style = case css:load_stylesheet(StylesheetPath) of
        StyleMap when map_size(StyleMap) =:= 0 ->
            logger:error("Failed to load stylesheet from ~s or file is empty, using default styles",
                        [StylesheetPath]),
            #{};
        LoadedStyle ->
            LoadedStyle
    end,

    {ok, #state{
        root_widget = container:new(root_widget, horizontal),
        last_root_widget = undefined,
        width = W,
        height = H,
        stylesheet = Style,
        force_redraw = true
    }}.

handle_call({set_root_widget, RootWidget}, _From, State) ->
    NewState = update(State#state{root_widget = RootWidget}),
    {reply, ok, NewState};

handle_call(get_root_widget, _From, State) ->
    {reply, State#state.root_widget, State};

handle_call(update_now, _From, State) ->
    NewState = update(State#state{force_redraw = true}),
    {reply, ok, NewState};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    NewState = update(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?TERMBOX:tb_shutdown(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% API
-doc "Sets the root widget tree and triggers a render if it has changed.".
set_root_widget(RootWidget) ->
    gen_server:call(?MODULE, {set_root_widget, RootWidget}).

-doc "Returns the current root widget tree.".
get_root_widget() ->
    gen_server:call(?MODULE, get_root_widget).

-doc "Forces an immediate re-render regardless of dirty state.".
update_now() ->
    gen_server:call(?MODULE, update_now).


%% INTERNAL
write_buffer_to_terminal(Buffer) ->
    maps:foreach(fun({X, Y}, {Char, Fg, Bg}) ->
        ?TERMBOX:tb_set_cell(X, Y, Char, Fg, Bg)
    end, Buffer).

update(State) ->
    W = ?TERMBOX:tb_width(),
    H = ?TERMBOX:tb_height(),
    RootWidget = State#state.root_widget,

    % Determine what changed
    SizeChanged = {W, H} =/= {State#state.width, State#state.height},
    WidgetChanged = RootWidget =/= State#state.last_root_widget,

    % Handle resize if needed
    ResizedState = case SizeChanged of
        true -> handle_resize(W, H, State);
        false -> State
    end,

    % Render if anything changed
    IsDirty = SizeChanged orelse WidgetChanged orelse State#state.force_redraw,
    case IsDirty of
        true -> render(W, H, RootWidget, ResizedState);
        false -> ResizedState
    end.

handle_resize(W, H, State) ->
    logger:info("View detected resize: ~p x ~p (was ~p x ~p)",
               [W, H, State#state.width, State#state.height]),
    ?TERMBOX:tb_clear(),
    ?TERMBOX:tb_force_redraw(),
    State#state{width = W, height = H}.

render(W, H, RootWidget, State) ->
    ?TERMBOX:tb_clear(),
    Layout = layout:calculate_layout(RootWidget, W, H),
    StyledLayout = css:style(Layout, State#state.stylesheet),
    Buffer = widgets:render(StyledLayout),
    write_buffer_to_terminal(Buffer),
    ?TERMBOX:tb_present(),
    State#state{last_root_widget = RootWidget, force_redraw = false}.
    
