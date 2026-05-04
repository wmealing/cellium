-module(cellium_dsl).
-moduledoc "The `cellium_dsl` module provides a Domain Specific Language (DSL) for defining UI layouts in Cellium.\n\n## How the DSL works\n\nThe DSL uses Erlang tuples to represent widgets and their properties. A UI layout is defined as a recursive structure of these tuples.\n\nThere are three main forms of DSL elements:\n1. **Standard leaf widgets**: `{Tag, Props}`\n   - Example: `{button, [{id, my_button}, {label, \"Click Me\"}]}`\n2. **Widgets with a primary value or containers**: `{Tag, Props, Arg3}`\n   - For containers like `vbox` or `hbox`, `Arg3` is a list of child DSL elements.\n   - For widgets like `button` or `text`, `Arg3` is the primary value (e.g., label string).\n3. **Custom widgets**: `{custom, Module, Props}`\n   - Allows using a custom module that implements a `new(Id)` function.\n\n## Recursive State Handling\n\nThe DSL transformation is state-aware. When calling `from_dsl/2`, you provide a `Model` which may contain a `widget_states` map.\n\nDuring the recursive traversal of the DSL:\n1. An `Id` is determined for each widget (either from `Props` or auto-generated).\n2. The widget is created.\n3. If the `Model` contains state for that `Id`, it is \"injected\" into the widget's internal map using `inject_state/3`. This allows the DSL to preserve UI state across re-renders.\n\n## Sizing and Expansion\n\nThe DSL handles layout properties that control how widgets occupy space:\n- `width`: Sets the fixed width of a widget.\n- `height` or `size`: Sets the fixed height (or primary dimension) of a widget.\n- `expand`: Indicates if a widget should grow to fill available space in its parent container.\n- **Constraints**: Fixed-size widgets should not exceed the available dimensions of their parent container.\n- **Defaults**: If neither a fixed size nor `expand` is specified, the DSL automatically applies a default `{size, 1}`.\n\n## Special Cases\n\nCertain tags have specialized behavior in the DSL:\n### Containers (`vbox`, `hbox`, `box`, `frame`, `dialog`)\nThese tags use the third element of the tuple as a list of children.\n### Tabs (`tabs`)\nThe `tabs` widget only renders the child corresponding to the `active_tab` property.\n### Radiogroup (`radiogroup`)\nThe `radiogroup` tag uses the third element of the tuple as the list of options.\n### Dropdown (`select`)\nThe `select` tag represents a dropdown widget. It can take options as the third element or in the `Props` list.\n### Leaf Widgets with Primary Values\nTags like `header`, `text`, `button`, etc., can take their primary display value as the third element for brevity.".
-export([from_dsl/1, from_dsl/2, validate/1]).

-doc """
Validates a DSL structure before processing.
Returns 'ok' or {error, {Reason, Path}}.
""".
validate(Dsl) ->
    validate_recursive(Dsl, []).

validate_recursive({custom, Module, Props}, Path) ->
    case is_atom(Module) andalso is_list(Props) of
        true -> validate_props(Props, [custom | Path]);
        false -> {error, {invalid_custom, Path}}
    end;

validate_recursive({Tag, Props, Arg3}, Path) ->
    CurrentPath = [Tag | Path],
    case is_atom(Tag) andalso is_known_tag(Tag) of
        true ->
            case is_list(Props) of
                true ->
                    case validate_arg3(Tag, Arg3, CurrentPath) of
                        ok -> validate_props(Props, CurrentPath);
                        Error -> Error
                    end;
                false -> {error, {invalid_props, CurrentPath}}
            end;
        false -> {error, {unknown_tag, CurrentPath}}
    end;

validate_recursive({Tag, Props}, Path) ->
    CurrentPath = [Tag | Path],
    case is_atom(Tag) andalso is_known_tag(Tag) of
        true ->
            case is_list(Props) of
                true -> validate_props(Props, CurrentPath);
                false -> {error, {invalid_props, CurrentPath}}
            end;
        false -> {error, {unknown_tag, CurrentPath}}
    end;

validate_recursive(Other, Path) ->
    {error, {invalid_dsl_format, {Other, Path}}}.

validate_arg3(Tag, Children, Path) when Tag=:=vbox; Tag=:=hbox; Tag=:=box; Tag=:=frame; Tag=:=dialog; Tag=:=tabs; Tag=:=stack ->
    if is_list(Children) ->
        lists:foldl(fun(C, ok) -> validate_recursive(C, Path);
                       (_, Err) -> Err
                    end, ok, Children);
       true -> {error, {expected_children_list, Path}}
    end;
validate_arg3(Tag, List, Path) when Tag=:=tree; Tag=:=radiogroup ->
    if is_list(List) -> ok;
       true -> 
           Reason = case Tag of
               tree -> expected_nodes_list;
               radiogroup -> expected_options_list
           end,
           {error, {Reason, Path}}
    end;
validate_arg3(Tag, _Value, _Path) when Tag=:=header; Tag=:=text; Tag=:=button; Tag=:=checkbox; Tag=:=radio; Tag=:=status_bar; Tag=:=select ->
    ok;
validate_arg3(_Tag, _Arg, Path) ->
    {error, {unexpected_3rd_argument, Path}}.

validate_props([], _Path) -> ok;
validate_props([{K, _V} | Rest], Path) when is_atom(K) -> validate_props(Rest, Path);
validate_props([P | _], Path) -> {error, {invalid_property, {P, Path}}}.
is_known_tag(Tag) ->
    lists:member(Tag, [
        vbox, hbox, box, frame, dialog, tabs, stack,
        header, text, button, checkbox, radio, status_bar,
        tree, select, list, progress_bar, toggle, spinner, spacer, gauge, table,
        radiogroup, stepper, text_input, breadcrumbs
    ]).

from_dsl(Dsl) -> from_dsl(Dsl, #{}).

%% Wrapper that extracts ID and finalizes the widget
from_dsl({custom, Module, Props}, Model) ->
    Id = get_id(Props),
    W = Module:new(Id),
    finalize_widget(W, Id, Props, Model);

from_dsl({Tag, Props, Arg3}, Model) when is_list(Props) ->
    Id = get_id(Props),
    W = create_widget(Tag, Id, Props, Arg3, Model),
    finalize_widget(W, Id, Props, Model);

from_dsl({Tag, Props}, Model) when is_list(Props) ->
    Id = get_id(Props),
    W = create_widget(Tag, Id, Props),
    finalize_widget(W, Id, Props, Model);

from_dsl(Other, _Model) ->
    logger:warning("Unknown DSL element: ~p", [Other]),
    (widget:new())#{id => make_ref(), widget_type => spacer, type => widget}.

%% Helper to get ID from props or generate a new one
get_id(Props) -> proplists:get_value(id, Props, make_ref()).

%% Finalize widget by injecting state and applying properties
finalize_widget(Widget, Id, Props, Model) ->
    W1 = inject_state(Widget, Id, Model),
    apply_leaf_props(W1, Props).

%% 3-tuple creation (Containers or widgets with primary value)
create_widget(Tag, Id, Props, Arg3, Model) ->
    case Tag of
        % Containers with children
        T when T=:=vbox; T=:=hbox; T=:=box; T=:=frame; T=:=dialog; T=:=stack ->
            W = case T of
                vbox -> vbox:new(Id);
                hbox -> hbox:new(Id);
                _    -> Tag:new(Id)
            end,
            IsContainer = (T =/= vbox andalso T =/= hbox),
            W1 = if IsContainer -> W#{type => container}; true -> W end,
            W1#{children => [from_dsl(C, Model) || C <- Arg3]};

        tabs ->
            ActiveIdx = proplists:get_value(active_tab, Props, 0),
            RealChildren = [
                case I == ActiveIdx of
                    true -> from_dsl(C, Model);
                    false -> (widget:new())#{widget_type => spacer, id => make_ref()}
                end
                || {I, C} <- lists:zip(lists:seq(0, length(Arg3)-1), Arg3)
                ],
            (tab:new(Id))#{children => RealChildren, type => container};

        % radiogroup takes its options list as Arg3
        radiogroup ->
            Orientation = proplists:get_value(orientation, Props, vertical),
            radiogroup:new(Id, Arg3, Orientation);

        select ->
            select:new(Id, Arg3);

        % Widgets with a primary value (label, text, etc.)
        T when T=:=header; T=:=text; T=:=button; T=:=checkbox; T=:=radio; T=:=status_bar; T=:=tree; T=:=stepper ->
            Tag:new(Id, Arg3);
        _ ->
            logger:warning("Tag ~p does not support 3rd argument value, ignoring.", [Tag]),
            create_widget(Tag, Id, Props)
    end.

%% 2-tuple creation (Standard leaf widgets)
create_widget(Tag, Id, Props) ->
    case Tag of
        T when T=:=header; T=:=text; T=:=button; T=:=checkbox; T=:=radio;
               T=:=progress_bar; T=:=toggle; T=:=text_input; T=:=spinner;
               T=:=box; T=:=frame; T=:=stack; T=:=spacer; T=:=gauge; T=:=table;
               T=:=stepper; T=:=breadcrumbs ->
            Tag:new(Id);
        radiogroup ->
            Options = proplists:get_value(options, Props, []),
            Orientation = proplists:get_value(orientation, Props, vertical),
            radiogroup:new(Id, Options, Orientation);
        select ->
            Options = proplists:get_value(options, Props, []),
            select:new(Id, Options);
        tabs -> tab:new(Id);
        list -> list:new(Id, proplists:get_value(items, Props, []));
        tree -> tree:new(Id, proplists:get_value(nodes, Props, []));
        _ ->
            logger:warning("Unknown widget tag: ~p", [Tag]),
            (widget:new())#{widget_type => spacer}
    end.

inject_state(Widget, Id, Model) ->
    States = maps:get(widget_states, Model, #{}),
    State = maps:get(Id, States, #{}),
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
    apply_props(Widget#{height => H, size => H, requested_height => H}, Rest);
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
