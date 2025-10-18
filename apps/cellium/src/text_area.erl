-module(text_area).

-export([render/1]).

render(_A) ->
	io:format("RENDERING ~p~n", [?MODULE]).
