-module(cellium_dsl).
-export([from_dsl/1, from_dsl/2]).

from_dsl(Dsl) -> from_dsl(Dsl, #{}).

from_dsl({vbox, Props, Children}, Model) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    C = container:new(Id, vertical),
    apply_leaf_props(inject_state(C#{children => [from_dsl(Child, Model) || Child <- Children]}, Id, Model), Props);

from_dsl({hbox, Props, Children}, Model) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    C = container:new(Id, horizontal),
    apply_leaf_props(inject_state(C#{children => [from_dsl(Child, Model) || Child <- Children]}, Id, Model), Props);

from_dsl({header, Props, Value}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = header:new(Id, Value),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({header, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = header:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({text, Props, Value}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = text:new(Id, Value),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({text, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = text:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({button, Props, Label}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = button:new(Id, Label),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({button, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = button:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({checkbox, Props, Label}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = checkbox:new(Id, Label),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({checkbox, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = checkbox:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({radio, Props, Label}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = radio:new(Id, Label),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({radio, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = radio:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({progress_bar, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = progress_bar:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({toggle, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = toggle:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({list, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    Items = proplists:get_value(items, Props, []),
    W = list:new(Id, Items),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({text_input, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = text_input:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({spinner, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = spinner:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({box, Props, Children}, Model) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = box:new(Id),
    apply_leaf_props(inject_state(W#{children => [from_dsl(Child, Model) || Child <- Children], type => container}, Id, Model), Props);

from_dsl({box, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = box:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({frame, Props, Children}, Model) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = frame:new(Id),
    apply_leaf_props(inject_state(W#{children => [from_dsl(Child, Model) || Child <- Children], type => container}, Id, Model), Props);

from_dsl({frame, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = frame:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({tabs, Props, Children}, Model) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    ActiveIdx = proplists:get_value(active_tab, Props, 0),
    
    % Only convert the active child to a widget tree to ensure only visible 
    % widgets are registered for focus.
    RealChildren = [ 
        case I == ActiveIdx of
            true -> from_dsl(C, Model);
            false -> (widget:new())#{widget_type => spacer, id => make_ref()}
        end
        || {I, C} <- lists:zip(lists:seq(0, length(Children)-1), Children)
    ],
    
    W = tab:new(Id),
    apply_leaf_props(inject_state(W#{children => RealChildren, type => container}, Id, Model), Props);

from_dsl({tabs, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = tab:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({spacer, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = spacer:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({dialog, Props, Children}, Model) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = dialog:new(Id),
    apply_leaf_props(inject_state(W#{children => [from_dsl(Child, Model) || Child <- Children], type => container}, Id, Model), Props);

from_dsl({gauge, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = gauge:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({table, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = table:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({status_bar, Props, Text}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = status_bar:new(Id, Text),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({custom_box, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = custom_box:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl({custom, Module, Props}, Model) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = Module:new(Id),
    apply_leaf_props(inject_state(W, Id, Model), Props);

from_dsl(Other, _Model) ->
    logger:warning("Unknown DSL element: ~p", [Other]),
    (widget:new())#{id => make_ref(), widget_type => spacer, type => widget}.


inject_state(Widget, Id, Model) ->
    States = maps:get(widget_states, Model, #{}),
    State = maps:get(Id, States, #{}),
    % Ensure we don't overwrite the Id or widget_type with whatever is in state
    % although they should match anyway.
    maps:merge(Widget, State).

apply_leaf_props(Widget, Props) ->
    HasSize = proplists:is_defined(size, Props) or proplists:is_defined(height, Props),
    HasExpand = proplists:is_defined(expand, Props),
    UpdatedProps = case HasSize orelse HasExpand of
        true -> Props;
        false -> [{size, 1} | Props]
    end,
    apply_props(Widget, UpdatedProps).

apply_props(Widget, []) -> widget:create(Widget);

apply_props(Widget, [{id, _} | Rest]) -> apply_props(Widget, Rest);

apply_props(Widget, [{padding, P} | Rest]) ->
    apply_props(Widget#{padding => parse_padding(P)}, Rest);

apply_props(Widget, [{width, W} | Rest]) ->
    apply_props(Widget#{width => W, requested_width => W}, Rest);

apply_props(Widget, [{height, H} | Rest]) ->
    apply_props(Widget#{size => H, requested_height => H}, Rest);

apply_props(Widget, [{size, S} | Rest]) ->
    apply_props(Widget#{size => S, requested_size => S}, Rest);

apply_props(Widget, [{Key, Val} | Rest]) ->
    apply_props(Widget#{Key => Val}, Rest).

parse_padding(N) when is_integer(N) ->
    #{top => N, bottom => N, left => N, right => N};
parse_padding(#{top := _, bottom := _, left := _, right := _} = P) ->
    P;
parse_padding(_) ->
    #{top => 0, bottom => 0, left => 0, right => 0}.
