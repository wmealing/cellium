-module(view).

-behaviour(gen_server).

-export([start_link/0, start_link/1, start_link/2, start_link/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).
-export([set_root_widget/1, update_now/0]).

-include("cellium.hrl").

-record(state, {
    root_widget = #{},
    last_root_widget = undefined,
    width = 0,
    height = 0,
    stylesheet = #{} ,
    force_redraw = false
}).

start_link(_A, _B, _C) ->
    start_link().

start_link(_A, _B) ->
    start_link().

start_link(_B) ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    _Initiated = ?TERMBOX:tb_init(),
    _Cleared = ?TERMBOX:tb_clear(),
    _Presented = ?TERMBOX:tb_present(),

    W = ?TERMBOX:tb_width(),
    H = ?TERMBOX:tb_height(),

    % Load stylesheet once at startup
    Style = css:load_stylesheet("priv/default_theme.css"),

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
set_root_widget(RootWidget) ->
  gen_server:call(?MODULE, {set_root_widget, RootWidget}).

update_now() ->
    gen_server:call(?MODULE, update_now).


%% INTERNAL
update(State) ->
    RootWidget = State#state.root_widget,

    W = ?TERMBOX:tb_width(),
    H = ?TERMBOX:tb_height(),

    % Check if terminal size changed
    {SizeChanged, State1} = case {W, H} =/= {State#state.width, State#state.height} of
        true ->
            logger:info("View detected resize: ~p x ~p (was ~p x ~p), forcing redraw", 
                        [W, H, State#state.width, State#state.height]),
            ?TERMBOX:tb_force_redraw(),
            {true, State#state{width = W, height = H}};
        false ->
            {false, State}
    end,

    % Dirty check: RootWidget changed, Size changed, or force_redraw requested
    IsDirty = SizeChanged
              orelse (RootWidget =/= State#state.last_root_widget)
              orelse (State#state.force_redraw == true),

    case IsDirty of
        false ->
            logger:debug("View update: Skipping redundant render (not dirty)"),
            State1;
        true ->
            logger:debug("View update: Rendering frame (dirty=~p, RootChanged=~p, Force=~p)", 
                         [IsDirty, RootWidget =/= State#state.last_root_widget, State#state.force_redraw]),
            ?TERMBOX:tb_clear(),
            Layout = layout:calculate_layout(RootWidget, W, H),
            StyledLayout = css:style(Layout, State1#state.stylesheet),
            widgets:render(StyledLayout),
            ?TERMBOX:tb_present(),
            State1#state{last_root_widget = RootWidget, force_redraw = false}
    end.
    
