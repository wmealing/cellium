-module(text_input).

-compile(export_all). 

render(_A) ->
	io:format("RENDERING ~p~n", [?MODULE]).
