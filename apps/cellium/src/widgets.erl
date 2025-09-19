-module(widgets).
-export([render/1]).

%% @doc Renders a widget into a virtual screen representation.

render(Widgets) when is_list(Widgets), length(Widgets) == 0 ->
    ok;

render(Widget) ->
    Name = maps:get(name, Widget, "no name set"),
    io:format("RENDERING WIDGET: ~p~n", [Name]),
    case maps:get(type, Widget) of
        container ->
            io:format("GOT CONTAINER, looking for children~n"),
            %% we need to render each of the children
            [render(Child) || Child <- maps:get(children, Widget, [])];
        Else ->
            io:format("GOT CHILD, SHOULD RENDER THE CHILD~n"),
            Else:render(Widget)

    end.
