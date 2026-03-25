-module(widgets).
-export([render/1, render_focused/1]).

%% @doc Renders a widget into a virtual screen representation.

render(Widgets) when is_list(Widgets), length(Widgets) == 0 ->
    ok;

render(Widget) ->
    case maps:get(type, Widget, none) of
        container ->
            Mod = maps:get(widget_type, Widget, container),
            case maps:get(focused, Widget, false) of
                true -> render_maybe_focused(Mod, Widget);
                false -> Mod:render(Widget)
            end,
            [render(Child) || Child <- maps:get(children, Widget, [])];
        widget ->
            Mod = maps:get(widget_type, Widget),
            case maps:get(focused, Widget, false) of
                true -> render_maybe_focused(Mod, Widget);
                false -> Mod:render(Widget)
            end;
        _ ->
            no_widgets
    end.

render_focused(Widget) ->
    Mod = maps:get(widget_type, Widget),
    render_maybe_focused(Mod, Widget).

render_maybe_focused(Mod, Widget) ->
    case erlang:function_exported(Mod, render_focused, 1) of
        true -> Mod:render_focused(Widget);
        false -> Mod:render(Widget)
    end.

