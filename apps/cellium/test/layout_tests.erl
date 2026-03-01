-module(layout_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test helper functions

create_simple_tree() ->
    #{
        id => root,
        type => container,
        children => [
            #{id => widget1, type => widget, data => value1},
            #{id => widget2, type => widget, data => value2}
        ]
    }.

create_nested_tree() ->
    #{
        id => root,
        type => container,
        children => [
            #{id => widget1, type => widget, data => value1},
            #{
                id => container1,
                type => container,
                children => [
                    #{id => widget2, type => widget, data => value2},
                    #{id => widget3, type => widget, data => value3}
                ]
            },
            #{id => widget4, type => widget, data => value4}
        ]
    }.

create_deep_nested_tree() ->
    #{
        id => root,
        type => container,
        children => [
            #{
                id => level1,
                type => container,
                children => [
                    #{
                        id => level2,
                        type => container,
                        children => [
                            #{id => deep_widget, type => widget, data => deep_value}
                        ]
                    }
                ]
            }
        ]
    }.

%% find/2 tests

find_root_test() ->
    Tree = create_simple_tree(),
    {ok, Found} = model:find(root, Tree),
    ?assertEqual(root, maps:get(id, Found)).

find_direct_child_test() ->
    Tree = create_simple_tree(),
    {ok, Found} = model:find(widget1, Tree),
    ?assertEqual(widget1, maps:get(id, Found)),
    ?assertEqual(value1, maps:get(data, Found)).

find_nested_child_test() ->
    Tree = create_nested_tree(),
    {ok, Found} = model:find(widget2, Tree),
    ?assertEqual(widget2, maps:get(id, Found)),
    ?assertEqual(value2, maps:get(data, Found)).

find_deeply_nested_test() ->
    Tree = create_deep_nested_tree(),
    {ok, Found} = model:find(deep_widget, Tree),
    ?assertEqual(deep_widget, maps:get(id, Found)),
    ?assertEqual(deep_value, maps:get(data, Found)).

find_container_test() ->
    Tree = create_nested_tree(),
    {ok, Found} = model:find(container1, Tree),
    ?assertEqual(container1, maps:get(id, Found)),
    ?assertEqual(container, maps:get(type, Found)).

find_nonexistent_test() ->
    Tree = create_simple_tree(),
    Result = model:find(nonexistent, Tree),
    ?assertEqual({error, not_found}, Result).

%% append/3 tests

append_to_root_test() ->
    Tree = create_simple_tree(),
    NewWidget = #{id => widget3, type => widget, data => value3},
    {ok, NewTree} = model:append(root, NewWidget, Tree),
    Children = maps:get(children, NewTree),
    ?assertEqual(3, length(Children)),
    {ok, Found} = model:find(widget3, NewTree),
    ?assertEqual(widget3, maps:get(id, Found)).

append_to_nested_container_test() ->
    Tree = create_nested_tree(),
    NewWidget = #{id => widget5, type => widget, data => value5},
    {ok, NewTree} = model:append(container1, NewWidget, Tree),
    {ok, Container} = model:find(container1, NewTree),
    Children = maps:get(children, Container),
    ?assertEqual(3, length(Children)),
    {ok, Found} = model:find(widget5, NewTree),
    ?assertEqual(value5, maps:get(data, Found)).

append_to_nonexistent_container_test() ->
    Tree = create_simple_tree(),
    NewWidget = #{id => widget3, type => widget},
    Result = model:append(nonexistent, NewWidget, Tree),
    ?assertEqual({error, not_found}, Result).

append_to_widget_test() ->
    Tree = create_simple_tree(),
    NewWidget = #{id => widget3, type => widget},
    Result = model:append(widget1, NewWidget, Tree),
    ?assertEqual({error, not_a_container}, Result).

append_multiple_items_test() ->
    Tree = create_simple_tree(),
    Widget3 = #{id => widget3, type => widget, data => value3},
    Widget4 = #{id => widget4, type => widget, data => value4},
    {ok, Tree2} = model:append(root, Widget3, Tree),
    {ok, Tree3} = model:append(root, Widget4, Tree2),
    Children = maps:get(children, Tree3),
    ?assertEqual(4, length(Children)).

%% replace/3 tests

replace_root_test() ->
    Tree = create_simple_tree(),
    NewRoot = #{id => root, type => container, data => new_data, children => []},
    {ok, NewTree} = model:replace(root, NewRoot, Tree),
    ?assertEqual(new_data, maps:get(data, NewTree)).

replace_direct_child_test() ->
    Tree = create_simple_tree(),
    NewWidget = #{id => widget1, type => widget, data => new_value},
    {ok, NewTree} = model:replace(widget1, NewWidget, Tree),
    {ok, Found} = model:find(widget1, NewTree),
    ?assertEqual(new_value, maps:get(data, Found)).

replace_nested_child_test() ->
    Tree = create_nested_tree(),
    NewWidget = #{id => widget2, type => widget, data => new_value2},
    {ok, NewTree} = model:replace(widget2, NewWidget, Tree),
    {ok, Found} = model:find(widget2, NewTree),
    ?assertEqual(new_value2, maps:get(data, Found)).

replace_container_test() ->
    Tree = create_nested_tree(),
    NewContainer = #{id => container1, type => container, data => container_data, children => []},
    {ok, NewTree} = model:replace(container1, NewContainer, Tree),
    {ok, Found} = model:find(container1, NewTree),
    ?assertEqual(container_data, maps:get(data, Found)),
    ?assertEqual([], maps:get(children, Found)).

replace_nonexistent_test() ->
    Tree = create_simple_tree(),
    NewWidget = #{id => nonexistent, type => widget},
    Result = model:replace(nonexistent, NewWidget, Tree),
    ?assertEqual({error, not_found}, Result).

replace_preserves_siblings_test() ->
    Tree = create_simple_tree(),
    NewWidget = #{id => widget1, type => widget, data => new_value},
    {ok, NewTree} = model:replace(widget1, NewWidget, Tree),
    {ok, Widget2} = model:find(widget2, NewTree),
    ?assertEqual(value2, maps:get(data, Widget2)).

%% delete/2 tests

delete_direct_child_test() ->
    Tree = create_simple_tree(),
    {ok, NewTree} = model:delete(widget1, Tree),
    Result = model:find(widget1, NewTree),
    ?assertEqual({error, not_found}, Result),
    Children = maps:get(children, NewTree),
    ?assertEqual(1, length(Children)).

delete_nested_child_test() ->
    Tree = create_nested_tree(),
    {ok, NewTree} = model:delete(widget2, Tree),
    Result = model:find(widget2, NewTree),
    ?assertEqual({error, not_found}, Result),
    {ok, Container} = model:find(container1, NewTree),
    Children = maps:get(children, Container),
    ?assertEqual(1, length(Children)).

delete_container_test() ->
    Tree = create_nested_tree(),
    {ok, NewTree} = model:delete(container1, Tree),
    ?assertEqual({error, not_found}, model:find(container1, NewTree)),
    ?assertEqual({error, not_found}, model:find(widget2, NewTree)),
    ?assertEqual({error, not_found}, model:find(widget3, NewTree)),
    {ok, Widget1} = model:find(widget1, NewTree),
    ?assertEqual(widget1, maps:get(id, Widget1)).

delete_root_test() ->
    Tree = create_simple_tree(),
    Result = model:delete(root, Tree),
    ?assertEqual({error, cannot_delete_root}, Result).

delete_nonexistent_test() ->
    Tree = create_simple_tree(),
    Result = model:delete(nonexistent, Tree),
    ?assertEqual({error, not_found}, Result).

delete_preserves_siblings_test() ->
    Tree = create_simple_tree(),
    {ok, NewTree} = model:delete(widget1, Tree),
    {ok, Widget2} = model:find(widget2, NewTree),
    ?assertEqual(value2, maps:get(data, Widget2)).

delete_all_children_test() ->
    Tree = create_simple_tree(),
    {ok, Tree2} = model:delete(widget1, Tree),
    {ok, Tree3} = model:delete(widget2, Tree2),
    Children = maps:get(children, Tree3),
    ?assertEqual(0, length(Children)).

%% update/3 tests

update_direct_child_test() ->
    Tree = create_simple_tree(),
    Updates = #{data => updated_value, extra => extra_data},
    {ok, NewTree} = model:update(widget1, Updates, Tree),
    {ok, Found} = model:find(widget1, NewTree),
    ?assertEqual(updated_value, maps:get(data, Found)),
    ?assertEqual(extra_data, maps:get(extra, Found)),
    ?assertEqual(widget, maps:get(type, Found)).

update_nested_child_test() ->
    Tree = create_nested_tree(),
    Updates = #{data => updated_value2},
    {ok, NewTree} = model:update(widget2, Updates, Tree),
    {ok, Found} = model:find(widget2, NewTree),
    ?assertEqual(updated_value2, maps:get(data, Found)).

update_root_test() ->
    Tree = create_simple_tree(),
    Updates = #{extra => root_extra},
    {ok, NewTree} = model:update(root, Updates, Tree),
    ?assertEqual(root_extra, maps:get(extra, NewTree)).

update_nonexistent_test() ->
    Tree = create_simple_tree(),
    Updates = #{data => value},
    Result = model:update(nonexistent, Updates, Tree),
    ?assertEqual({error, not_found}, Result).

update_multiple_fields_test() ->
    Tree = create_simple_tree(),
    Updates = #{data => new_data, visible => true, x => 10, y => 20},
    {ok, NewTree} = model:update(widget1, Updates, Tree),
    {ok, Found} = model:find(widget1, NewTree),
    ?assertEqual(new_data, maps:get(data, Found)),
    ?assertEqual(true, maps:get(visible, Found)),
    ?assertEqual(10, maps:get(x, Found)),
    ?assertEqual(20, maps:get(y, Found)).

%% get_parent/2 tests

get_parent_of_direct_child_test() ->
    Tree = create_simple_tree(),
    {ok, Parent} = model:get_parent(widget1, Tree),
    ?assertEqual(root, maps:get(id, Parent)).

get_parent_of_nested_child_test() ->
    Tree = create_nested_tree(),
    {ok, Parent} = model:get_parent(widget2, Tree),
    ?assertEqual(container1, maps:get(id, Parent)).

get_parent_of_container_test() ->
    Tree = create_nested_tree(),
    {ok, Parent} = model:get_parent(container1, Tree),
    ?assertEqual(root, maps:get(id, Parent)).

get_parent_of_root_test() ->
    Tree = create_simple_tree(),
    Result = model:get_parent(root, Tree),
    ?assertEqual({error, no_parent}, Result).

get_parent_of_nonexistent_test() ->
    Tree = create_simple_tree(),
    Result = model:get_parent(nonexistent, Tree),
    ?assertEqual({error, no_parent}, Result).

%% find_path/2 tests

find_path_to_root_test() ->
    Tree = create_simple_tree(),
    {ok, Path} = model:find_path(root, Tree),
    ?assertEqual([root], Path).

find_path_to_direct_child_test() ->
    Tree = create_simple_tree(),
    {ok, Path} = model:find_path(widget1, Tree),
    ?assertEqual([root, widget1], Path).

find_path_to_nested_child_test() ->
    Tree = create_nested_tree(),
    {ok, Path} = model:find_path(widget2, Tree),
    ?assertEqual([root, container1, widget2], Path).

find_path_to_deep_nested_test() ->
    Tree = create_deep_nested_tree(),
    {ok, Path} = model:find_path(deep_widget, Tree),
    ?assertEqual([root, level1, level2, deep_widget], Path).

find_path_to_nonexistent_test() ->
    Tree = create_simple_tree(),
    Result = model:find_path(nonexistent, Tree),
    ?assertEqual({error, not_found}, Result).

%% Integration tests

complex_workflow_test() ->
    Tree = create_simple_tree(),
    NewWidget = #{id => widget3, type => widget, data => value3},
    {ok, Tree2} = model:append(root, NewWidget, Tree),
    {ok, Tree3} = model:update(widget1, #{data => updated}, Tree2),
    {ok, Tree4} = model:delete(widget2, Tree3),
    Children = maps:get(children, Tree4),
    ?assertEqual(2, length(Children)),
    {ok, Widget1} = model:find(widget1, Tree4),
    ?assertEqual(updated, maps:get(data, Widget1)),
    {ok, Widget3} = model:find(widget3, Tree4),
    ?assertEqual(value3, maps:get(data, Widget3)),
    ?assertEqual({error, not_found}, model:find(widget2, Tree4)).

nested_container_workflow_test() ->
    Tree = create_nested_tree(),
    NewContainer = #{id => container2, type => container, children => []},
    {ok, Tree2} = model:append(root, NewContainer, Tree),
    NewWidget = #{id => widget5, type => widget, data => value5},
    {ok, Tree3} = model:append(container2, NewWidget, Tree2),
    {ok, Found} = model:find(widget5, Tree3),
    ?assertEqual(value5, maps:get(data, Found)),
    {ok, Path} = model:find_path(widget5, Tree3),
    ?assertEqual([root, container2, widget5], Path).

replace_and_verify_structure_test() ->
    Tree = create_nested_tree(),
    NewContainer = #{
        id => container1,
        type => container,
        children => [
            #{id => new_widget1, type => widget, data => new_value1}
        ]
    },
    {ok, NewTree} = model:replace(container1, NewContainer, Tree),
    ?assertEqual({error, not_found}, model:find(widget2, NewTree)),
    ?assertEqual({error, not_found}, model:find(widget3, NewTree)),
    {ok, NewWidget} = model:find(new_widget1, NewTree),
    ?assertEqual(new_value1, maps:get(data, NewWidget)).

%% Edge case tests

empty_container_test() ->
    Tree = #{id => root, type => container, children => []},
    {ok, Found} = model:find(root, Tree),
    ?assertEqual(root, maps:get(id, Found)),
    Result = model:find(anything, Tree),
    ?assertEqual({error, not_found}, Result).

single_widget_tree_test() ->
    Tree = #{id => single, type => widget, data => value},
    {ok, Found} = model:find(single, Tree),
    ?assertEqual(value, maps:get(data, Found)).

append_to_empty_container_test() ->
    Tree = #{id => root, type => container, children => []},
    NewWidget = #{id => widget1, type => widget},
    {ok, NewTree} = model:append(root, NewWidget, Tree),
    Children = maps:get(children, NewTree),
    ?assertEqual(1, length(Children)).

many_children_test() ->
    ManyWidgets = [#{id => list_to_atom("widget" ++ integer_to_list(N)), 
                     type => widget} || N <- lists:seq(1, 100)],
    Tree = #{id => root, type => container, children => ManyWidgets},
    {ok, Found} = model:find(widget50, Tree),
    ?assertEqual(widget50, maps:get(id, Found)).
