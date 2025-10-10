-module(status_bar).

-compile([render/1]). 

render(_A) ->
	io:format("RENDERING ~p~n", [?MODULE]).
