-module(widgets).
-export([render/1, render/2, render_focused/1, render_focused/2]).

%% @doc Renders a widget into a virtual screen representation.
%% This is the entry point that initializes an empty buffer.
render(Widget) ->
    render(Widget, cellium_buffer:empty()).

render(Widgets, Buffer) when is_list(Widgets), length(Widgets) == 0 ->
    Buffer;

render(Widget, Buffer) ->
    case maps:get(type, Widget, none) of
        container ->
            Mod = maps:get(widget_type, Widget, container),
            Buffer1 = case maps:get(focused, Widget, false) of
                true -> render_maybe_focused(Mod, Widget, Buffer);
                false -> Mod:render(Widget, Buffer)
            end,
            lists:foldl(fun(Child, AccBuffer) ->
                render(Child, AccBuffer)
            end, Buffer1, maps:get(children, Widget, []));
        widget ->
            Mod = maps:get(widget_type, Widget),
            case maps:get(focused, Widget, false) of
                true -> render_maybe_focused(Mod, Widget, Buffer);
                false -> Mod:render(Widget, Buffer)
            end;
        _ ->
            Buffer
    end.

render_focused(Widget) ->
    render_focused(Widget, cellium_buffer:empty()).

render_focused(Widget, Buffer) ->
    Mod = maps:get(widget_type, Widget),
    render_maybe_focused(Mod, Widget, Buffer).

render_maybe_focused(Mod, Widget, Buffer) ->
    case erlang:function_exported(Mod, render_focused, 2) of
        true -> Mod:render_focused(Widget, Buffer);
        false -> Mod:render(Widget, Buffer)
    end.

