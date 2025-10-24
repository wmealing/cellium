-module(widgets).
-export([render/1]).

%% @doc Renders a widget into a virtual screen representation.

render(Widgets) when is_list(Widgets), length(Widgets) == 0 ->
    ok;

render(Widget) ->
    case maps:get(type, Widget, none) of
        container ->
            Mod = maps:get(widget_type, Widget, container),
            Mod:render(Widget),
            [render(Child) || Child <- maps:get(children, Widget, [])];
        widget ->
            Mod = maps:get(widget_type, Widget),
            Mod:render(Widget);
        _ ->
            no_widgets
    end.
