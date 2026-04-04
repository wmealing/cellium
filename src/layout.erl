-module(layout).
-export([calculate_layout/1, calculate_layout/3]).

-include("cellium.hrl").

-spec calculate_layout(map(), integer(), integer()) -> map().
calculate_layout(Widget, Width, Height) ->
    NewWidget = Widget#{width => Width,
                        height => Height,
                        parent_width => Width,
                        parent_height => Height},
    calculate_layout(NewWidget).

-spec calculate_layout(map()) -> map().
calculate_layout(#{position := absolute} = Widget) ->
    %% Absolutely positioned widgets skip layout for themselves
    %% but we must layout their children using their OWN dimensions as parent bounds.
    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),
    do_calculate_layout(Widget#{parent_width => Width, parent_height => Height});

calculate_layout(#{position := centered} = Widget) ->
    %% Centered widgets are positioned relative to the screen dimensions
    W_Total = ?TERMBOX:tb_width(),
    H_Total = ?TERMBOX:tb_height(),
    
    Width = maps:get(requested_width, Widget, maps:get(width, Widget, 40)),
    Height = maps:get(requested_height, Widget, maps:get(height, Widget, maps:get(size, Widget, 10))),
    
    X = max(0, (W_Total - Width) div 2),
    Y = max(0, (H_Total - Height) div 2),
    
    % Force calculated dimensions and layout children relative to this box
    CenteredWidget = Widget#{x => X, y => Y, width => Width, height => Height, 
                            position => absolute, parent_width => Width, parent_height => Height},
    do_calculate_layout(CenteredWidget);

calculate_layout(OriginalContainer) ->
    do_calculate_layout(OriginalContainer).

do_calculate_layout(OriginalContainer) ->
    Children = maps:get(children, OriginalContainer, []),
    Orientation = maps:get(orientation, OriginalContainer, horizontal),
    Id = maps:get(id, OriginalContainer, undefined),
    WidgetType = maps:get(widget_type, OriginalContainer, undefined),

    FocusedWidgetId = case whereis(focus_manager) of
        undefined -> undefined;
        _ -> 
            case focus_manager:get_focused() of
                {ok, FW} -> FW;
                _ -> undefined
            end
    end,

    IsFocused = (Id =/= undefined) andalso (Id =:= FocusedWidgetId),
    Container = OriginalContainer#{focused => IsFocused, has_focus => IsFocused},

    DefaultPadding = case WidgetType of
        frame -> #{top => 1, right => 1, bottom => 1, left => 1};
        dialog -> #{top => 1, right => 1, bottom => 1, left => 1};
        _ -> #{top => 0, right => 0, bottom => 0, left => 0}
    end,

    Padding = maps:get(padding, Container, DefaultPadding),
    PaddingTop = maps:get(top, Padding, 0),
    PaddingRight = maps:get(right, Padding, 0),
    PaddingBottom = maps:get(bottom, Padding, 0),
    PaddingLeft = maps:get(left, Padding, 0),

    X = maps:get(x, Container, 0),
    Y = maps:get(y, Container, 0),
    
    ContainerWidth = maps:get(width, Container, maps:get(parent_width, Container, ?TERMBOX:tb_width())),
    ContainerHeight = maps:get(height, Container, maps:get(parent_height, Container, ?TERMBOX:tb_height())),

    Width = max(0, ContainerWidth - PaddingLeft - PaddingRight),
    Height = max(0, ContainerHeight - PaddingTop - PaddingBottom),

    if Children == [] ->
            Container;
       true ->
            InnerX = X + PaddingLeft,
            InnerY = Y + PaddingTop,
            
            if WidgetType == tab orelse WidgetType == dialog ->
                RealizedChildren = lists:map(fun(Child) ->
                    C1 = Child#{x => InnerX, y => InnerY, width => Width, height => Height,
                                 parent_width => Width, parent_height => Height},
                    case maps:get(type, C1) of
                        container -> calculate_layout(C1);
                        widget -> 
                            ChildId = maps:get(id, C1, undefined),
                            IsChildFocused = (ChildId =/= undefined) andalso (ChildId =:= FocusedWidgetId),
                            C1#{focused => IsChildFocused, has_focus => IsChildFocused}
                    end
                end, Children),
                Container#{children => RealizedChildren};
            true ->
                ExpandChildren = [Child || Child <- Children, maps:is_key(expand, Child)],
                FixedChildren = [Child || Child <- Children,
                            maps:is_key(size, Child) andalso not maps:is_key(expand, Child)],

                case Orientation of
                    horizontal ->
                        ParentSize = Width,
                        FixedSizeSum = lists:sum([maps:get(size, Child) || Child <- FixedChildren]),
                        AvailableSpace = ParentSize - FixedSizeSum;
                    vertical ->
                        ParentSize = Height,
                        FixedSizeSum = lists:sum([maps:get(size, Child) || Child <- FixedChildren]),
                        AvailableSpace = ParentSize - FixedSizeSum
                end,

                ExpandCount = length(ExpandChildren),

                ExpandedChildren =
                    if ExpandCount > 0 ->
                        BaseExpandSize = trunc(AvailableSpace / ExpandCount),
                        Remainder = AvailableSpace rem ExpandCount,

                        lists:map(fun(Child) -> Child#{size => BaseExpandSize} end,
                                lists:sublist(ExpandChildren, 1, ExpandCount - 1))
                        ++ [(lists:last(ExpandChildren))#{size => BaseExpandSize + Remainder}];
                    true ->
                        []
                    end,

                UpdatedChildren =
                    lists:foldl(
                        fun(Child, Acc) ->
                            case maps:is_key(expand, Child) of
                                true ->
                                    [ExpandedChild] = [EC || EC <- ExpandedChildren, maps:get(id, EC) == maps:get(id, Child)],
                                    Acc ++ [ExpandedChild];
                                false ->
                                    Acc ++ [Child]
                            end
                        end, [], Children),

                {RealizedChildren, _} =
                    lists:foldl(fun({_Index, Child}, {Acc, CurrentOffset}) ->

                        IsAbsolute = maps:get(position, Child, relative) =:= absolute orelse maps:get(position, Child, relative) =:= centered,

                        ChildSize = maps:get(size, Child, 0),
                        Gap = 0,

                        RealizedChild = case IsAbsolute of
                            true ->
                                Child;
                            false ->
                                case Orientation of
                                    horizontal ->
                                        Child#{x => InnerX + CurrentOffset + Gap, y => InnerY, width => ChildSize, height => Height,
                                               parent_width => Width, parent_height => Height};
                                    vertical ->
                                        Child#{x => InnerX, y => InnerY + CurrentOffset + Gap, width => Width, height => ChildSize,
                                               parent_width => Width, parent_height => Height}
                                end
                        end,
                        RealizedChildContainer =
                            case maps:get(type, RealizedChild) of
                                container ->
                                    calculate_layout(RealizedChild);
                                widget ->
                                    ChildId = maps:get(id, RealizedChild, undefined),
                                    IsChildFocused = (ChildId =/= undefined) andalso (ChildId =:= FocusedWidgetId),
                                    RealizedChild#{focused => IsChildFocused, has_focus => IsChildFocused}
                            end,

                        {Acc ++ [RealizedChildContainer], CurrentOffset + Gap + ChildSize}
                    end, {[], 0}, lists:zip(lists:seq(0, length(UpdatedChildren) - 1), UpdatedChildren)),

                Container#{children => RealizedChildren}
            end
    end.
