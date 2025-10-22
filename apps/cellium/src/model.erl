-module(model).

-export([
    find/2,
    append/3,
    replace/3,
    delete/2,
    update/3,
    get_parent/2,
    find_path/2,
    maybe_set_focus/1
]).

%% @doc Find a widget or container by ID in the layout tree.
%% Returns {ok, Item} if found, {error, not_found} otherwise.
-spec find(Id :: term(), Tree :: map()) -> {ok, map()} | {error, not_found}.
find(Id, Tree) ->
    find_recursive(Id, Tree).

%% @doc Append an item to a container identified by ContainerId.
%% Returns {ok, NewTree} if successful, {error, Reason} otherwise.
-spec append(ContainerId :: term(), Item :: map(), Tree :: map()) -> 
    {ok, map()} | {error, term()}.
append(ContainerId, Item, Tree) ->
    case find(ContainerId, Tree) of
        {ok, Container} ->
            case is_container(Container) of
                true ->
                    UpdatedContainer = append_to_children(Container, Item),
                    replace(ContainerId, UpdatedContainer, Tree);
                false ->
                    {error, not_a_container}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Replace an item in the tree identified by Id with NewItem.
%% Returns {ok, NewTree} if successful, {error, Reason} otherwise.
-spec replace(Id :: term(), NewItem :: map(), Tree :: map()) -> 
    {ok, map()} | {error, term()}.
replace(Id, NewItem, Tree) ->
    replace_recursive(Id, NewItem, Tree).

%% @doc Delete an item from the tree identified by Id.
%% Returns {ok, NewTree} if successful, {error, Reason} otherwise.
-spec delete(Id :: term(), Tree :: map()) -> {ok, map()} | {error, term()}.
delete(Id, Tree) ->
    TreeId = maps:get(id, Tree, undefined),
    if
        TreeId =:= Id ->
            {error, cannot_delete_root};
        true ->
            delete_recursive(Id, Tree)
    end.

%% @doc Update specific fields of an item identified by Id.
%% Returns {ok, NewTree} if successful, {error, Reason} otherwise.
-spec update(Id :: term(), Updates :: map(), Tree :: map()) -> 
    {ok, map()} | {error, term()}.
update(Id, Updates, Tree) ->
    case find(Id, Tree) of
        {ok, Item} ->
            UpdatedItem = maps:merge(Item, Updates),
            replace(Id, UpdatedItem, Tree);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get the parent container of an item identified by Id.
%% Returns {ok, Parent} if found, {error, Reason} otherwise.
-spec get_parent(Id :: term(), Tree :: map()) -> {ok, map()} | {error, term()}.
get_parent(Id, Tree) ->
    case find_parent_recursive(Id, Tree, undefined) of
        undefined ->
            {error, no_parent};
        Parent ->
            {ok, Parent}
    end.

%% @doc Get the path from root to an item identified by Id.
%% Returns {ok, [Id1, Id2, ..., Id]} if found, {error, not_found} otherwise.
-spec find_path(Id :: term(), Tree :: map()) -> {ok, [term()]} | {error, not_found}.
find_path(Id, Tree) ->
    case find_path_recursive(Id, Tree, []) of
        [] ->
            {error, not_found};
        Path ->
            {ok, lists:reverse(Path)}
    end.

%% Internal helper functions

find_recursive(Id, Tree) when is_map(Tree) ->
    TreeId = maps:get(id, Tree, undefined),
    if
        TreeId =:= Id ->
            {ok, Tree};
        true ->
            find_in_children(Id, Tree)
    end;
find_recursive(_Id, _Tree) ->
    {error, not_found}.

find_in_children(Id, Container) ->
    Children = maps:get(children, Container, []),
    find_in_list(Id, Children).

find_in_list(_Id, []) ->
    {error, not_found};
find_in_list(Id, [Child | Rest]) ->
    case find_recursive(Id, Child) of
        {ok, Found} ->
            {ok, Found};
        {error, not_found} ->
            find_in_list(Id, Rest)
    end.

replace_recursive(Id, NewItem, Tree) when is_map(Tree) ->
    TreeId = maps:get(id, Tree, undefined),
    if
        TreeId =:= Id ->
            {ok, NewItem};
        true ->
            replace_in_children(Id, NewItem, Tree)
    end.

replace_in_children(Id, NewItem, Container) ->
    case maps:is_key(children, Container) of
        true ->
            Children = maps:get(children, Container),
            case replace_in_list(Id, NewItem, Children) of
                {ok, NewChildren} ->
                    {ok, Container#{children => NewChildren}};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, not_found}
    end.

replace_in_list(_Id, _NewItem, []) ->
    {error, not_found};
replace_in_list(Id, NewItem, [Child | Rest]) ->
    ChildId = maps:get(id, Child, undefined),
    if
        ChildId =:= Id ->
            {ok, [NewItem | Rest]};
        true ->
            case replace_recursive(Id, NewItem, Child) of
                {ok, UpdatedChild} ->
                    {ok, [UpdatedChild | Rest]};
                {error, not_found} ->
                    case replace_in_list(Id, NewItem, Rest) of
                        {ok, UpdatedRest} ->
                            {ok, [Child | UpdatedRest]};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

delete_recursive(Id, Tree) when is_map(Tree) ->
    case delete_from_children(Id, Tree) of
        {ok, UpdatedTree} ->
            {ok, UpdatedTree};
        {error, Reason} ->
            {error, Reason}
    end.

delete_from_children(Id, Container) ->
    case maps:is_key(children, Container) of
        true ->
            Children = maps:get(children, Container),
            case delete_from_list(Id, Children) of
                {ok, NewChildren} ->
                    {ok, Container#{children => NewChildren}};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, not_found}
    end.

delete_from_list(_Id, []) ->
    {error, not_found};
delete_from_list(Id, [Child | Rest]) ->
    ChildId = maps:get(id, Child, undefined),
    if
        ChildId =:= Id ->
            {ok, Rest};
        true ->
            case delete_recursive(Id, Child) of
                {ok, UpdatedChild} ->
                    {ok, [UpdatedChild | Rest]};
                {error, not_found} ->
                    case delete_from_list(Id, Rest) of
                        {ok, UpdatedRest} ->
                            {ok, [Child | UpdatedRest]};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

find_parent_recursive(Id, Tree, _CurrentParent) when is_map(Tree) ->
    Children = maps:get(children, Tree, []),
    case has_child_with_id(Id, Children) of
        true ->
            Tree;
        false ->
            find_parent_in_children(Id, Children, Tree)
    end;
find_parent_recursive(_Id, _Tree, _CurrentParent) ->
    undefined.

find_parent_in_children(_Id, [], _Parent) ->
    undefined;
find_parent_in_children(Id, [Child | Rest], Parent) ->
    case find_parent_recursive(Id, Child, Parent) of
        undefined ->
            find_parent_in_children(Id, Rest, Parent);
        Found ->
            Found
    end.

has_child_with_id(Id, Children) ->
    lists:any(fun(Child) ->
        maps:get(id, Child, undefined) =:= Id
    end, Children).

find_path_recursive(Id, Tree, Acc) when is_map(Tree) ->
    TreeId = maps:get(id, Tree, undefined),
    NewAcc = [TreeId | Acc],
    if
        TreeId =:= Id ->
            NewAcc;
        true ->
            Children = maps:get(children, Tree, []),
            find_path_in_children(Id, Children, NewAcc)
    end;
find_path_recursive(_Id, _Tree, _Acc) ->
    [].

find_path_in_children(_Id, [], _Acc) ->
    [];
find_path_in_children(Id, [Child | Rest], Acc) ->
    case find_path_recursive(Id, Child, Acc) of
        [] ->
            find_path_in_children(Id, Rest, Acc);
        Path ->
            Path
    end.

is_container(Item) ->
    Type = maps:get(type, Item, undefined),
    Type =:= container.

append_to_children(Container, Item) ->
    Children = maps:get(children, Container, []),
    Container#{children => Children ++ [Item]}.


maybe_set_focus(Widget) ->
    {ok, FocusedWidgetId} = focus_manager:get_focused(),
    MyWidgetID = maps:get(id, Widget, no_id),
    case FocusedWidgetId == MyWidgetID of
        true ->
            %% set the focused attribute
            Widget#{ has_focus => true };
        _ ->
            Widget
    end.

