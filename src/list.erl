%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(list).
-moduledoc """
List widget module for rendering scrollable lists of items.

This module provides an interactive list widget that handles keyboard
events for selection and scrolling.

## Usage

Basic list:
```
{list, [{id, my_list}, {items, ["Item 1", "Item 2", "Item 3"]}]}
```

## Properties

- `items` (list of strings): The items to display in the list.
- `selected_index` (integer): The index of the currently selected item.
- `scroll_offset` (integer): The index of the first item to display.
- `focusable` (boolean): Always true for lists.
- `focused` (boolean): Set by focus manager when the list has focus.
""".

-export([render/2, render_focused/2, new/1, new/2, handle_event/2, calculate_scroll_offset/3]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new list widget with no items.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, []).

-doc "Creates a new list widget with the specified items.".
-spec new(term(), [string()]) -> map().
new(Id, Items) ->
    (widget:new())#{id => Id,
                    widget_type => list,
                    items => Items,
                    selected_index => 0,
                    scroll_offset => 0,
                    focusable => true,
                    type => widget}.

-doc """
Handles keyboard events for the list.

Processes arrow keys for selection and scrolling.
""".
-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, up_key}, State) ->
    SelectedIndex = maps:get(selected_index, State, 0),
    NewIndex = max(0, SelectedIndex - 1),
    update_state_with_scroll(State#{selected_index => NewIndex});
handle_event({key, _, _, _, _, down_key}, State) ->
    Items = maps:get(items, State, []),
    SelectedIndex = maps:get(selected_index, State, 0),
    NewIndex = min(length(Items) - 1, SelectedIndex + 1),
    update_state_with_scroll(State#{selected_index => NewIndex});
handle_event(_, State) ->
    State.

update_state_with_scroll(State) ->
    case maps:get(height, State, 0) of
        H when H > 0 ->
            CurrentOffset = maps:get(scroll_offset, State, 0),
            SelectedIndex = maps:get(selected_index, State, 0),
            NewOffset = calculate_scroll_offset(SelectedIndex, CurrentOffset, H),
            State#{scroll_offset => NewOffset};
        _ ->
            State
    end.

-doc "Renders the list widget.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    do_render(Widget, Buffer).

-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    do_render(Widget, Buffer).

do_render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Items = maps:get(items, Widget, []),
    SelectedIndex = maps:get(selected_index, Widget, 0),
    Height = maps:get(height, Widget, 0),
    Width = maps:get(width, Widget, 0),
    HasFocus = maps:get(focused, Widget, false),

    if
        Height > 0 andalso Width > 0 ->
            % 1. Calculate the CORRECT scroll offset for this render
            % We use the one from the model as the starting point
            CurrentOffset = maps:get(scroll_offset, Widget, 0),
            ScrollOffset = calculate_scroll_offset(SelectedIndex, CurrentOffset, Height),
            
            VisibleItems = lists:sublist(Items, ScrollOffset + 1, Height),
            
            lists:foldl(fun({Idx, Item}, AccBuffer) ->
                ActualIdx = ScrollOffset + Idx - 1,
                IsSelected = (ActualIdx == SelectedIndex),
                
                {ItemFg, ItemBg} = if IsSelected andalso HasFocus -> {Bg, Fg};
                                      IsSelected -> {cyan, Bg};
                                      true -> {Fg, Bg}
                                   end,
                
                % Convert item to string if needed
                ItemStr = if is_binary(Item) -> binary_to_list(Item);
                             is_list(Item) -> Item;
                             true -> io_lib:format("~p", [Item])
                          end,
                
                % Truncate item to width
                FinalItem = truncate_string(ItemStr, Width),
                cellium_buffer:put_string(X, Y + Idx - 1, ItemFg, ItemBg, FinalItem, AccBuffer)
            end, Buffer, lists:zip(lists:seq(1, length(VisibleItems)), VisibleItems));
        true ->
            Buffer
    end.

calculate_scroll_offset(SelectedIndex, CurrentOffset, Height) ->
    if
        SelectedIndex < CurrentOffset -> SelectedIndex;
        SelectedIndex >= CurrentOffset + Height -> SelectedIndex - Height + 1;
        true -> CurrentOffset
    end.

truncate_string(Str, MaxLen) ->
    if length(Str) > MaxLen -> lists:sublist(Str, MaxLen);
       true -> Str
    end.
