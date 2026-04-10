-module(widgets).
-export([render/1, render/2, render_overlays/2, render_focused/1, render_focused/2]).

%% @doc Renders a widget into a virtual screen representation.
%% This is the entry point that initializes an empty buffer.
render(Widget) ->
    render(Widget, cellium_buffer:empty()).

render(Widgets, Buffer) when is_list(Widgets), length(Widgets) == 0 ->
    Buffer;

render(Widget, Buffer) ->
    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),
    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),
    
    % Save the incoming clip to restore it later
    OldClip = maps:get(clip, Buffer, undefined),
    
    % Intersect current clip with this widget's bounds
    BufferWithClip = cellium_buffer:set_clip(X, Y, Width, Height, Buffer),
    
    ResultBuffer = case maps:get(type, Widget, none) of
        container ->
            Mod = maps:get(widget_type, Widget, container),
            Buffer1 = case maps:get(focused, Widget, false) of
                true -> render_maybe_focused(Mod, Widget, BufferWithClip);
                false -> Mod:render(Widget, BufferWithClip)
            end,
            
            % Render children. Note: each child will set/restore its own clip.
            Children = case Mod of
                tab ->
                    ActiveIdx = maps:get(active_tab, Widget, 0),
                    AllChildren = maps:get(children, Widget, []),
                    if ActiveIdx >= 0 andalso ActiveIdx < length(AllChildren) ->
                        [lists:nth(ActiveIdx + 1, AllChildren)];
                    true ->
                        []
                    end;
                _ ->
                    maps:get(children, Widget, [])
            end,
            lists:foldl(fun(Child, AccBuffer) ->
                render(Child, AccBuffer)
            end, Buffer1, Children);
        widget ->
            Mod = maps:get(widget_type, Widget),
            case maps:get(focused, Widget, false) of
                true -> render_maybe_focused(Mod, Widget, BufferWithClip);
                false -> Mod:render(Widget, BufferWithClip)
            end;
        _ ->
            BufferWithClip
    end,
    
    % Restore the parent's clip so siblings aren't affected by our bounds
    case OldClip of
        undefined -> maps:remove(clip, ResultBuffer);
        _ -> ResultBuffer#{clip => OldClip}
    end.

%% @doc Renders overlays for widgets that need to draw over the top of others.
%% This is the second pass of the rendering process.
render_overlays(Widgets, Buffer) when is_list(Widgets) ->
    lists:foldl(fun(Widget, AccBuffer) ->
        render_overlays(Widget, AccBuffer)
    end, Buffer, Widgets);

render_overlays(Widget, Buffer) ->
    ResultBuffer = case maps:get(widget_type, Widget, none) of
        none -> Buffer;
        Mod ->
            case erlang:function_exported(Mod, render_overlay, 2) of
                true -> Mod:render_overlay(Widget, Buffer);
                false -> Buffer
            end
    end,

    % Recurse into children regardless of whether this widget has an overlay
    case maps:get(children, Widget, undefined) of
        Children when is_list(Children) ->
            lists:foldl(fun(Child, AccBuffer) ->
                render_overlays(Child, AccBuffer)
            end, ResultBuffer, Children);
        _ ->
            ResultBuffer
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
