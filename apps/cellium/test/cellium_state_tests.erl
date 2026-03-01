%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@Wades-MacBook-Air.local>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created :  6 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(cellium_state_tests).
-include_lib("eunit/include/eunit.hrl").

%% Define setup and teardown functions
setup() ->
    io:format("Setup: Initializing resources...~n"),
    {ok, "resource_handle"}.

teardown(ResourceHandle) ->
    io:format("Teardown: Cleaning up resources: ~p~n", [ResourceHandle]),
    ok.

%% Test suite with setup and teardown
my_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(ResourceHandle) ->  % Instantiator function that receives the resource
         [
          ?_test(my_first_test(ResourceHandle)),
          ?_test(my_second_test(ResourceHandle))
         ]
     end}.

%% Individual test functions that receive the resource handle
my_first_test(ResourceHandle) ->
    io:format("Running my_first_test with resource: ~p~n", [ResourceHandle]),
    ?assertEqual({ok, "resource_handle"}, ResourceHandle).

my_second_test(ResourceHandle) ->
    io:format("Running my_second_test with resource: ~p~n", [ResourceHandle]),
    ?assertMatch({ok, _}, ResourceHandle).
