-module(css).

-export([style/1, style/2, walk_layout/2, load_stylesheet/1, parse_stylesheet/1, parse_value/1, parse_properties/1]).

-export([default_stylesheet/0]).

%% Main API - applies default styles
style(Layout) ->
    Stylesheet = default_stylesheet(),
    style(Layout, Stylesheet).

%% Apply stylesheet to layout
style(Layout, Stylesheet) ->
    walk_layout(Layout, fun(Widget) -> apply_styles(Widget, Stylesheet) end).

%% Walk the layout tree and apply a function to each widget
walk_layout(Widget, Fun) when is_map(Widget) ->
    UpdatedWidget = Fun(Widget),
    walk_children(UpdatedWidget, Fun);
walk_layout(Widget, _Fun) ->
    Widget.

%% Load stylesheet from file
load_stylesheet(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            parse_stylesheet(Content);
        {error, _Reason} ->
            #{} 
    end.

%% Parse CSS-like stylesheet format
%% Example:
%% .button { color: 2; expand: true; }
%% #my_widget { color: 5; width: 100; }
parse_stylesheet(Content) ->
    %% Remove line breaks and collapse multi-line rules into single lines
    Collapsed = collapse_multiline_rules(Content),
    Lines = string:tokens(Collapsed, "\n"),
    CleanLines = lists:map(fun string:strip/1, Lines),
    parse_rules(CleanLines, #{}).

%% Private functions

walk_children(Widget, Fun) ->
    case maps:get(children, Widget, undefined) of
        undefined ->
            Widget;
        Children when is_list(Children) ->
            UpdatedChildren = lists:map(fun(Child) -> walk_layout(Child, Fun) end, Children),
            Widget#{children => UpdatedChildren};
        _ ->
            Widget
    end.

apply_styles(Widget, Stylesheet) ->
    IdStyles = get_id_styles(Widget, Stylesheet),
    ClassStyles = get_class_styles(Widget, Stylesheet),
    merge_styles(Widget, ClassStyles, IdStyles).

get_id_styles(Widget, Stylesheet) ->
    case maps:get(id, Widget, undefined) of
        undefined -> #{};
        Id -> maps:get({id, Id}, Stylesheet, #{})
    end.

get_class_styles(Widget, Stylesheet) ->
    case maps:get(class, Widget, undefined) of
        undefined -> #{};
        Class when is_atom(Class) ->
            maps:get({class, Class}, Stylesheet, #{});
        Classes when is_list(Classes) ->
            merge_class_list(Classes, Stylesheet, #{} )
    end.

merge_class_list([], _Stylesheet, Acc) ->
    Acc;
merge_class_list([Class | Rest], Stylesheet, Acc) ->
    ClassStyles = maps:get({class, Class}, Stylesheet, #{}),
    NewAcc = maps:merge(Acc, ClassStyles),
    merge_class_list(Rest, Stylesheet, NewAcc).

merge_styles(Widget, ClassStyles, IdStyles) ->
    maps:merge(maps:merge(Widget, ClassStyles), IdStyles).

parse_rules([], Acc) ->
    Acc;
parse_rules([Line | Rest], Acc) ->
    case parse_selector_line(Line) of
        {Selector, Properties} ->
            NewAcc = maps:put(Selector, Properties, Acc),
            parse_rules(Rest, NewAcc);
        skip ->
            parse_rules(Rest, Acc)
    end.

%% Collapse multi-line CSS rules into single lines
%% Converts:
%%   .button {
%%     color: 2;
%%   }
%% To: .button { color: 2; }
collapse_multiline_rules(Content) ->
    %% Replace newlines within braces with spaces
    collapse_between_braces(Content, [], false).

collapse_between_braces([], Acc, _InBraces) ->
    lists:reverse(Acc);
collapse_between_braces([${ | Rest], Acc, _InBraces) ->
    collapse_between_braces(Rest, [${ | Acc], true);
collapse_between_braces([$} | Rest], Acc, _InBraces) ->
    %% Add closing brace and newline to separate rules
    collapse_between_braces(Rest, [$\n, $} | Acc], false);
collapse_between_braces([$\n | Rest], Acc, true) ->
    %% Inside braces: replace newline with space
    collapse_between_braces(Rest, [$\s | Acc], true);
collapse_between_braces([C | Rest], Acc, InBraces) ->
    collapse_between_braces(Rest, [C | Acc], InBraces).

parse_selector_line(Line) ->
    Stripped = string:strip(Line),
    case Stripped of
        "" -> skip;
        [$# | _] = IdLine -> parse_id_selector(IdLine);
        [$. | _] = ClassLine -> parse_class_selector(ClassLine);
        _ -> skip
    end.

parse_id_selector(Line) ->
    case string:chr(Line, ${) of
        0 -> skip;
        Pos ->
            IdPart = string:substr(Line, 2, Pos - 2),
            Id = list_to_atom(string:strip(IdPart)),
            PropsStr = extract_properties(Line, Pos),
            Props = parse_properties(PropsStr),
            {{id, Id}, Props}
    end.

parse_class_selector(Line) ->
    case string:chr(Line, ${) of
        0 -> skip;
        Pos ->
            ClassPart = string:substr(Line, 2, Pos - 2),
            Class = list_to_atom(string:strip(ClassPart)),
            PropsStr = extract_properties(Line, Pos),
            Props = parse_properties(PropsStr),
            {{class, Class}, Props}
    end.

extract_properties(Line, StartPos) ->
    case string:chr(Line, $}) of
        0 -> "";
        EndPos ->
            string:substr(Line, StartPos + 1, EndPos - StartPos - 1)
    end.

parse_properties(PropsStr) ->
    Statements = string:tokens(PropsStr, ";"),
    lists:foldl(fun parse_property_statement/2, #{}, Statements).

parse_property_statement(Statement, Acc) ->
    case string:tokens(Statement, ":") of
        [KeyStr, ValueStr] ->
            Key = list_to_atom(string:strip(KeyStr)),
            Value = parse_value(string:strip(ValueStr)),
            maps:put(Key, Value, Acc);
        _ ->
            Acc
    end.

parse_value(ValueStr) ->
    Stripped = string:strip(ValueStr),
    case string:to_integer(Stripped) of
        {Int, ""} -> Int;
        _ ->
            case Stripped of
                "true" -> true;
                "false" -> false;
                _ -> list_to_atom(Stripped)
            end
    end.

default_stylesheet() ->
    #{
        {class, primary_button} => #{
            color => 2,
            expand => true
        },
        {class, secondary_button} => #{
            color => 3,
            expand => false
        },
        {class, container_box} => #{
            color => 7,
            expand => true
        },
        {id, main_container} => #{
            expand => true
        }
    }.
