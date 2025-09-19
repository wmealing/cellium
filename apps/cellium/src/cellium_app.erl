%%%-------------------------------------------------------------------
%% @doc cellium public API
%% @end
%%%-------------------------------------------------------------------

-module(cellium_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cellium_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
