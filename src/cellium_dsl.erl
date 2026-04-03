-module(cellium_dsl).
-export([from_dsl/1]).

from_dsl({vbox, Props, Children}) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    C = container:new(Id, vertical),
    apply_leaf_props(C#{children => [from_dsl(Child) || Child <- Children]}, Props);

from_dsl({hbox, Props, Children}) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    C = container:new(Id, horizontal),
    apply_leaf_props(C#{children => [from_dsl(Child) || Child <- Children]}, Props);

from_dsl({header, Props, Value}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = header:new(Id, Value),
    apply_leaf_props(W, Props);

from_dsl({text, Props, Value}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = text:new(Id, Value),
    apply_leaf_props(W, Props);

from_dsl({button, Props, Label}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = button:new(Id, Label),
    apply_leaf_props(W, Props);

from_dsl({checkbox, Props, Label}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = checkbox:new(Id, Label),
    apply_leaf_props(W, Props);

from_dsl({radio, Props, Label}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = radio:new(Id, Label),
    apply_leaf_props(W, Props);

from_dsl({progress_bar, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = progress_bar:new(Id),
    apply_leaf_props(W, Props);

from_dsl({toggle, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = toggle:new(Id),
    apply_leaf_props(W, Props);

from_dsl({text_input, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = text_input:new(Id),
    apply_leaf_props(W, Props);

from_dsl({spinner, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = spinner:new(Id),
    apply_leaf_props(W, Props);

from_dsl({box, Props, Children}) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = box:new(Id),
    apply_leaf_props(W#{children => [from_dsl(Child) || Child <- Children], type => container}, Props);

from_dsl({box, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = box:new(Id),
    apply_leaf_props(W, Props);

from_dsl({frame, Props, Children}) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = frame:new(Id),
    apply_leaf_props(W#{children => [from_dsl(Child) || Child <- Children], type => container}, Props);

from_dsl({frame, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = frame:new(Id),
    apply_leaf_props(W, Props);

from_dsl({tabs, Props, Children}) when is_list(Children) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = tab:new(Id),
    apply_leaf_props(W#{children => [from_dsl(Child) || Child <- Children], type => container}, Props);

from_dsl({tabs, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = tab:new(Id),
    apply_leaf_props(W, Props);

from_dsl({spacer, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = spacer:new(Id),
    apply_leaf_props(W, Props);

from_dsl({gauge, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = gauge:new(Id),
    apply_leaf_props(W, Props);

from_dsl({status_bar, Props, Text}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = status_bar:new(Id, Text),
    apply_leaf_props(W, Props);

from_dsl({custom, Module, Props}) ->
    Id = proplists:get_value(id, Props, make_ref()),
    W = Module:new(Id),
    apply_leaf_props(W, Props);

from_dsl(Other) ->
    logger:warning("Unknown DSL element: ~p", [Other]),
    (widget:new())#{id => make_ref(), widget_type => spacer, type => widget}.


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
