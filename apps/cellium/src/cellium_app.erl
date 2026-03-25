%%%-------------------------------------------------------------------
%% @doc cellium public API
%% @end
%%%-------------------------------------------------------------------

-module(cellium_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("cellium.hrl"). 

start(_StartType, _StartArgs) ->
    logging:setup(),
    logger:info("Starting focus manager"),
    cellium_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
