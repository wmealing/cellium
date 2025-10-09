-module(status_bar).

-compile(export_all). 

render(_A) ->
	io:format("RENDERING ~p~n", [?MODULE]).
