-module(view).

-behaviour(gen_server).

-export([start_link/0, start_link/1, start_link/2, start_link/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).
-export([set_root_widget/1, update_now/0]).

-include("cellium.hrl").

-define(TICK_INTERVAL, 1000). % ms

-record(state, { root_widget = #{}}).

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

    % Start periodic tick
    erlang:send_after(50, self(), tick),

    {ok, #state{root_widget = container:new(root_widget, horizontal)}}.

handle_call({set_root_widget, RootWidget}, _From, State) ->
    NewState = State#state{root_widget = RootWidget},
    update(NewState),
    {reply, ok, NewState};

handle_call(update_now, _From, State) ->
    update(State),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
%   Schedule next tick
%   erlang:send_after(?TICK_INTERVAL, self(), tick),
    update(State),
    {noreply, State};

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

    Layout = layout:calculate_layout(RootWidget,
                                     ?TERMBOX:tb_width() - 1,
                                     ?TERMBOX:tb_height() - 1),

    Style = css:load_stylesheet("priv/default_theme.css"),

    StyledLayout = css:style(Layout, Style),
    logger:info("STYLED LAYOUT: ~p~n", [StyledLayout]),

    ?TERMBOX:tb_clear(),
    widgets:render(StyledLayout),
    ?TERMBOX:tb_present().
