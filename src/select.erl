-module(select).
-moduledoc """
A dropdown select widget that displays a list of options over the top of other widgets.

When closed, it displays the currently selected item. When activated (Enter), it opens
 a boxed list of options. The user can navigate these options with arrow keys and 
confirm the selection with Enter.
""".

-export([render/2, render_overlay/2, render_focused/2, handle_event/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, []).

-spec new(term(), [term()]) -> map().
new(Id, Options) ->
    (widget:new())#{id => Id,
                    widget_type => select,
                    options => Options,
                    selected_index => 0,
                    is_open => false,
                    focusable => true,
                    type => widget}.

-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, enter_key}, #{is_open := true, id := Id, options := Options, selected_index := Idx} = State) ->
    Selected = lists:nth(Idx + 1, Options),
    self() ! {select_changed, Id, Selected},
    State#{is_open => false};
handle_event({key, _, _, _, _, enter_key}, #{is_open := false} = State) ->
    State#{is_open => true};
handle_event({key, _, _, _, _, esc_key}, #{is_open := true} = State) ->
    State#{is_open => false};
handle_event({key, _, _, _, _, up_key}, #{is_open := true, selected_index := Idx} = State) ->
    State#{selected_index => max(0, Idx - 1)};
handle_event({key, _, _, _, _, down_key}, #{is_open := true, selected_index := Idx, options := Options} = State) ->
    State#{selected_index => min(length(Options) - 1, Idx + 1)};
handle_event(_Event, State) ->
    State.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Options = maps:get(options, Widget, []),
    Idx = maps:get(selected_index, Widget, 0),
    MaxLen = get_max_len(Options),
    
    SelectedText = case Options of
        [] -> "None";
        _ -> 
            Val = lists:nth(Idx + 1, Options),
            S = to_list(Val),
            S ++ string:copies(" ", MaxLen - length(S))
    end,
    
    Display = io_lib:format("[ ~ts ~ts ]", [SelectedText, [16#25be]]),
    
    cellium_buffer:put_string(X, Y, Fg, Bg, lists:flatten(Display), Buffer).

-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Options = maps:get(options, Widget, []),
    Idx = maps:get(selected_index, Widget, 0),
    MaxLen = get_max_len(Options),
    
    SelectedText = case Options of
        [] -> "None";
        _ -> 
            Val = lists:nth(Idx + 1, Options),
            S = to_list(Val),
            S ++ string:copies(" ", MaxLen - length(S))
    end,
    
    Display = io_lib:format("[ ~ts ~ts ]", [SelectedText, [16#25be]]),
    
    % Invert colors for focus
    cellium_buffer:put_string(X, Y, Bg, Fg, lists:flatten(Display), Buffer).

-spec render_overlay(map(), map()) -> map().
render_overlay(#{is_open := false}, Buffer) ->
    Buffer;
render_overlay(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Options = maps:get(options, Widget, []),
    SelectedIndex = maps:get(selected_index, Widget, 0),
    
    % Find longest option for width
    MaxLen = get_max_len(Options),
    
    % Draw box
    B1 = draw_box(X, Y + 1, MaxLen + 4, length(Options) + 2, Fg, Bg, Buffer),
    
    % Draw options
    lists:foldl(fun({Idx, Opt}, AccBuffer) ->
        IsSelected = (Idx == SelectedIndex),
        {ItemFg, ItemBg} = if IsSelected -> {Bg, cyan}; true -> {Fg, Bg} end,
        OptStr = " " ++ to_list(Opt) ++ string:copies(" ", MaxLen + 1 - length(to_list(Opt))),
        cellium_buffer:put_string(X + 1, Y + 2 + Idx, ItemFg, ItemBg, OptStr, AccBuffer)
    end, B1, lists:zip(lists:seq(0, length(Options) - 1), Options)).

get_max_len(Options) ->
    lists:foldl(fun(Opt, Acc) ->
        L = length(to_list(Opt)),
        if L > Acc -> L; true -> Acc end
    end, 5, Options).

draw_box(X, Y, W, H, Fg, Bg, Buffer) ->
    % Simple box drawing with unicode
    % Top and bottom
    B1 = lists:foldl(fun(I, Acc) ->
        A1 = cellium_buffer:set_cell(X + I, Y, [16#2500], Fg, Bg, Acc),
        cellium_buffer:set_cell(X + I, Y + H - 1, [16#2500], Fg, Bg, A1)
    end, Buffer, lists:seq(1, W - 2)),
    
    % Sides
    B2 = lists:foldl(fun(I, Acc) ->
        A1 = cellium_buffer:set_cell(X, Y + I, [16#2502], Fg, Bg, Acc),
        cellium_buffer:set_cell(X + W - 1, Y + I, [16#2502], Fg, Bg, A1)
    end, B1, lists:seq(1, H - 2)),
    
    % Corners
    B3 = cellium_buffer:set_cell(X, Y, [16#250c], Fg, Bg, B2),
    B4 = cellium_buffer:set_cell(X + W - 1, Y, [16#2510], Fg, Bg, B3),
    B5 = cellium_buffer:set_cell(X, Y + H - 1, [16#2514], Fg, Bg, B4),
    cellium_buffer:set_cell(X + W - 1, Y + H - 1, [16#2518], Fg, Bg, B5).

to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_list(V) -> V;
to_list(V) -> io_lib:format("~p", [V]).
