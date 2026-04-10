-module(cellium_dsl).
-export([from_dsl/1, from_dsl/2]).

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
        T when T=:=vbox; T=:=hbox; T=:=box; T=:=frame; T=:=dialog ->
            W = case T of
                vbox -> container:new(Id, vertical);
                hbox -> container:new(Id, horizontal);
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

        % Widgets with a primary value (label, text, etc.)
        T when T=:=header; T=:=text; T=:=button; T=:=checkbox; T=:=radio; T=:=status_bar ->
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
               T=:=box; T=:=frame; T=:=spacer; T=:=gauge; T=:=table ->
            Tag:new(Id);
        radiogroup ->
            Options = proplists:get_value(options, Props, []),
            Orientation = proplists:get_value(orientation, Props, vertical),
            radiogroup:new(Id, Options, Orientation);
        tabs -> tab:new(Id);
        list -> list:new(Id, proplists:get_value(items, Props, []));
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
