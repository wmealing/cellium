%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(tree).
-moduledoc """
Tree widget module for rendering hierarchical data.

This module provides an interactive tree widget that handles keyboard
events for navigation, expansion, and selection.

## Usage

Basic tree:
```
{tree, [{id, my_tree}, {nodes, [
    {"Root", [
        {"Child 1", []},
        {"Child 2", [
            {"Grandchild", []}
        ]}
    ]}
]}]}
```

## Properties

- `nodes` (list): The tree structure.
- `expanded_ids` (list): IDs of nodes that are currently expanded.
- `selected_index` (integer): The index of the currently selected visible item.
- `scroll_offset` (integer): The index of the first item to display.
""".

-export([render/2, render_focused/2, new/1, new/2, handle_event/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new tree widget.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, []).

-doc "Creates a new tree widget with the specified nodes.".
-spec new(term(), list()) -> map().
new(Id, Nodes) ->
    (widget:new())#{id => Id,
                    widget_type => tree,
                    nodes => Nodes,
                    expanded_ids => [],
                    selected_index => 0,
                    scroll_offset => 0,
                    focusable => true,
                    type => widget}.

-doc """
Handles keyboard events for the tree.
""".
-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, up_key}, State) ->
    SelectedIndex = maps:get(selected_index, State, 0),
    NewIndex = max(0, SelectedIndex - 1),
    update_state_with_scroll(State#{selected_index => NewIndex});
handle_event({key, _, _, _, _, down_key}, State) ->
    Nodes = maps:get(nodes, State, []),
    ExpandedIds = maps:get(expanded_ids, State, []),
    Flattened = flatten(Nodes, ExpandedIds),
    SelectedIndex = maps:get(selected_index, State, 0),
    NewIndex = min(length(Flattened) - 1, SelectedIndex + 1),
    update_state_with_scroll(State#{selected_index => NewIndex});
handle_event({key, _, _, _, _, right_key}, State) ->
    Nodes = maps:get(nodes, State, []),
    ExpandedIds = maps:get(expanded_ids, State, []),
    Flattened = flatten(Nodes, ExpandedIds),
    SelectedIndex = maps:get(selected_index, State, 0),
    case lists:nth(SelectedIndex + 1, Flattened) of
        {Node, _, false, true} ->
            Id = get_node_id(Node),
            State#{expanded_ids => [Id | ExpandedIds]};
        _ ->
            State
    end;
handle_event({key, _, _, _, _, left_key}, State) ->
    Nodes = maps:get(nodes, State, []),
    ExpandedIds = maps:get(expanded_ids, State, []),
    Flattened = flatten(Nodes, ExpandedIds),
    SelectedIndex = maps:get(selected_index, State, 0),
    case lists:nth(SelectedIndex + 1, Flattened) of
        {Node, _, true, _} ->
            Id = get_node_id(Node),
            State#{expanded_ids => lists:delete(Id, ExpandedIds)};
        _ ->
            State
    end;
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

calculate_scroll_offset(SelectedIndex, CurrentOffset, Height) ->
    if
        SelectedIndex < CurrentOffset ->
            SelectedIndex;
        SelectedIndex >= CurrentOffset + Height ->
            SelectedIndex - Height + 1;
        true ->
            CurrentOffset
    end.

-doc "Renders the tree widget.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    do_render(Widget, Buffer).

-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    do_render(Widget, Buffer).

do_render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Nodes = maps:get(nodes, Widget, []),
    ExpandedIds = maps:get(expanded_ids, Widget, []),
    SelectedIndex = maps:get(selected_index, Widget, 0),
    Height = maps:get(height, Widget, 0),
    Width = maps:get(width, Widget, 0),
    HasFocus = maps:get(focused, Widget, false),

    if
        Height > 0 andalso Width > 0 ->
            Flattened = flatten(Nodes, ExpandedIds),
            CurrentOffset = maps:get(scroll_offset, Widget, 0),
            ScrollOffset = calculate_scroll_offset(SelectedIndex, CurrentOffset, Height),
            
            VisibleItems = lists:sublist(Flattened, ScrollOffset + 1, Height),
            
            {FinalBuffer, _} = lists:foldl(fun({Item, Depth, IsExpanded, HasChildren}, {AccBuffer, AccY}) ->
                IsSelected = (AccY - Y + ScrollOffset == SelectedIndex),
                
                ItemFg = if IsSelected andalso HasFocus -> black; true -> Fg end,
                ItemBg = if IsSelected andalso HasFocus -> Fg; true -> Bg end,
                
                % Clear the line
                Buffer1 = cellium_buffer:put_string(X, AccY, Fg, Bg, lists:duplicate(Width, $\s), AccBuffer),
                
                Prefix = if 
                    HasChildren andalso IsExpanded -> "  ▾ ";
                    HasChildren -> "  ▸ ";
                    true -> "    "
                end,
                Indentation = lists:duplicate(Depth * 2, $\s),
                Label = get_node_label(Item),
                Text = Indentation ++ Prefix ++ Label,
                
                % Draw the item text
                Buffer2 = cellium_buffer:put_string(X, AccY, ItemFg, ItemBg, lists:sublist(Text, Width), Buffer1),
                {Buffer2, AccY + 1}
            end, {Buffer, Y}, VisibleItems),
            FinalBuffer;
        true ->
            Buffer
    end.

%% Internal helpers

flatten(Nodes, ExpandedIds) ->
    flatten(Nodes, ExpandedIds, 0, []).

flatten([], _, _, Acc) ->
    Acc;
flatten([Node | Rest], ExpandedIds, Depth, Acc) ->
    Id = get_node_id(Node),
    Children = get_node_children(Node),
    HasChildren = Children =/= [],
    IsExpanded = lists:member(Id, ExpandedIds),
    
    NewAcc = Acc ++ [{Node, Depth, IsExpanded, HasChildren}],
    
    FinalAcc = if
        IsExpanded andalso HasChildren ->
            flatten(Children, ExpandedIds, Depth + 1, NewAcc);
        true ->
            NewAcc
    end,
    flatten(Rest, ExpandedIds, Depth, FinalAcc).

get_node_id({Id, _Label, _Children}) -> Id;
get_node_id({Label, _Children}) -> Label;
get_node_id(Label) when is_list(Label); is_binary(Label) -> Label.

get_node_label({_Id, Label, _Children}) -> Label;
get_node_label({Label, _Children}) -> Label;
get_node_label(Label) when is_list(Label); is_binary(Label) -> Label.

get_node_children({_Id, _Label, Children}) -> Children;
get_node_children({_Label, Children}) -> Children;
get_node_children(_) -> [].
