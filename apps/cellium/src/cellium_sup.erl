%%%-------------------------------------------------------------------
%% @doc cellium top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cellium_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
   %% ----------------------------------------------------
    %% 1. Supervisor Flags (Strategy)
    %% ----------------------------------------------------
    %% Strategy: one_for_one (default) - If a child dies, only that child is restarted.
    %% Intensity: 10 - Max number of restarts.
    %% Period: 5 - Time period in seconds for the intensity check.
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },

    %% ----------------------------------------------------
    %% 2. Child Specifications (Worker Processes)
    %% ----------------------------------------------------

    %% Child 1: cellium_state
    %% NOTE: This specification explicitly calls the custom start function: cellium_state:start_link_local/0
    %% StateWorker = #{
    %%     id => cellium_state,
    %%     start => {cellium_state, start_link_local, []}, %% The specified function call
    %%     restart => permanent,                          %% Always restart if it dies
    %%     type => worker,
    %%     shutdown => 5000                               %% Max time in ms for a graceful shutdown
    %% },

    %% %% Child 2: cellium_event_manager
    %% EventManager = #{
    %%     id => cellium_event_manager,
    %%     start => {cellium_event_manager, start_link, []},
    %%     restart => permanent,
    %%     type => worker,
    %%     shutdown => 5000
    %% },

    %% %% %% Child 3: cellium_render_server
    %% RenderServer = #{
    %%     id => cellium_render_server,
    %%     start => {cellium_renderer_server, start_link, []},
    %%     restart => permanent,
    %%     type => worker,
    %%     shutdown => 5000
    %% },

    ChildSpecs = [
%                  RenderServer,
%                  StateWorker,
%                  EventManager
                 ],

    {ok, {SupFlags, ChildSpecs}}.
