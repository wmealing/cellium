-module(widgets).
-export([render/1]).

%% A tuple representing a virtual screen drawing command.
-type virtual_screen_data() :: list({put_string, {integer(), integer()}, unicode:chardata(), list()}).

-record(bbox, {x, y, width, height}).

%% @doc Renders a widget into a virtual screen representation.
render(Widget) ->
    io:format("RENDERING WIDGET..~n"),
    io:format("IS MAP? ~p~n", [is_map(Widget)]),
    io:format("WIDGET IS: ~p~n", [Widget]),
    
    timer:sleep(10000),
    case maps:get(type, Widget) of
        container ->
            io:format("GOT CONTAINER, looking for children");
        _Else ->
            io:format("GOT CHILD, SHOULD RENDER THE CHILD")
    end.
