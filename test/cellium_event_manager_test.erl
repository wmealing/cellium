-module (cellium_event_manager_test).

-include_lib("eunit/include/eunit.hrl").

-export([first_test/0]).


layout_test_() ->
    [
     ?_test(first_test())
    ].


first_test() ->
	ok.
