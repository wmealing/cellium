%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(app_event_manager).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

-include("cellium.hrl").
-include_lib("eunit/include/eunit.hrl").

% API
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

-export([event_input/1,set_model/1, get_model/0]).

% Callbacks
-export([init/1,
              handle_call/3,
              handle_cast/2,
              handle_info/2,
              terminate/2,
              code_change/3]).

-record(state, {model=#{}}).

% API

% see: http://erlang.org/doc/man/gen_server.html#start_link-3
start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->
    start_link(#{}).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

setup_logging() ->
    logger:remove_handler(default),
    logger:set_primary_config(level, debug),
    Config = #{config => #{file => "./logs/debug"}, level => debug},
    logger:add_handler(to_file_handler, logger_std_h, Config),
    logger:info("App Event Manager logging started.").

% Callbacks
init(Model) ->
    setup_logging(),
    view:start_link(),
    view:set_root_widget(Model),
    {ok, #state{model=Model}}.

handle_call({set_model, Model}, _From, _State) ->
    view:set_root_widget(Model),
    NewState = #state{model=Model},
    {reply, ok, NewState};

handle_call({get_model}, _From, #state{model=Model} = State) ->
    Return = {ok, Model},
    {reply, Return, State};

handle_call(Msg, _From, State) ->
    logger:info("Unknown event: ~p~n", [Msg]),

    %% get the Model
    #state{model=Model} = State,

    %% find the table, change an attribute
    {ok,Table} = model:find(table_demo, Model),
    logger:info("Found Table: ~p~n", [Table]),

    %% this doesn't seem to take.
    {ok, NewModel} = model:update(table_demo, #{style => rounded}, Model),


    NewState = #state{model=NewModel},

    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
event_input(Event) ->
    gen_server:call(?MODULE, Event).

set_model(NewState) ->
    gen_server:call(?MODULE, {set_state, NewState}).

get_model() ->
    gen_server:call(?MODULE, {get_state}).
