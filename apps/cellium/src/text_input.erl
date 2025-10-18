-module(text_input).


-export([render/1]).

render(_A) ->
	io:format("RENDERING ~p~n", [?MODULE]).
