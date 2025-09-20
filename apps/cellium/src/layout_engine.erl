-module(layout_engine).
-export([calculate_layout/3, partition_children/1, calculate_non_flex_dimensions/1, calculate_flex_dimensions/4, position_children/8, update_child/5]).
-include("cellium.hrl").

-doc "Calculates the layout for a container and its children.".
-spec calculate_layout(map(), integer(), integer()) -> map().
calculate_layout(Container, Width, Height) ->
    calculate_layout(Container, 0, 0, Width, Height).

-spec calculate_layout(map(), integer(), integer(), integer(), integer()) -> map().
calculate_layout(Container, X, Y, Width, Height) ->
    Orientation = maps:get(orientation, Container),
    Children = maps:get(children, Container, []),

    {FlexChildren, NonFlexChildren} = partition_children(Children),

    {UsedWidth, UsedHeight} = calculate_non_flex_dimensions(NonFlexChildren),

    RemainingWidth = Width - UsedWidth,
    RemainingHeight = Height - UsedHeight,

    {FlexWidths, FlexHeights} = calculate_flex_dimensions(FlexChildren, RemainingWidth, RemainingHeight, Orientation),

    UpdatedChildren = position_children(Children, X, Y, Width, Height, Orientation, FlexWidths, FlexHeights),

    Container#{children => UpdatedChildren, x => X, y => Y, width => Width, height => Height}.


-doc "Partitions a list of children into flex and non-flex children.".
-spec partition_children(list()) -> {list(), list()}.
partition_children(Children) ->
    FlexChildren = [Child || Child <- Children, maps:get(flex, Child, 0) > 0],
    NonFlexChildren = [Child || Child <- Children, maps:get(flex, Child, 0) =:= 0],
    {FlexChildren, NonFlexChildren}.

-doc "Calculates the total width and height of non-flex children.".
-spec calculate_non_flex_dimensions(list()) -> {integer(), integer()}.
calculate_non_flex_dimensions(NonFlexChildren) ->
    lists:foldl(
        fun(Child, {AccWidth, AccHeight}) ->
            {AccWidth + maps:get(width, Child, 0), AccHeight + maps:get(height, Child, 0)}
        end,
        {0, 0},
        NonFlexChildren
    ).

-doc "Calculates the width and height for each flex child, distributing any remainder.".
-spec calculate_flex_dimensions(list(), integer(), integer(), atom()) -> {list(), list()}.
calculate_flex_dimensions(FlexChildren, RemainingWidth, RemainingHeight, Orientation) ->
    NumFlex = length(FlexChildren),
    if
        NumFlex > 0, Orientation =:= horizontal ->
            BaseWidth = RemainingWidth div NumFlex,
            Remainder = RemainingWidth rem NumFlex,
            Widths = [BaseWidth + if I < Remainder -> 1; true -> 0 end || I <- lists:seq(0, NumFlex - 1)],
            {Widths, []};
        NumFlex > 0, Orientation =:= vertical ->
            BaseHeight = RemainingHeight div NumFlex,
            Remainder = RemainingHeight rem NumFlex,
            Heights = [BaseHeight + if I < Remainder -> 1; true -> 0 end || I <- lists:seq(0, NumFlex - 1)],
            {[], Heights};
        true ->
            {[], []}
    end.

-doc "Positions the children and recursively calculates the layout for containers.".
-spec position_children(list(), integer(), integer(), integer(), integer(), atom(), list(), list()) -> list().
position_children(Children, X, Y, Width, Height, Orientation, FlexWidths, FlexHeights) ->
    NumChildren = length(Children),
    {_, _, UpdatedChildren, _, _} = lists:foldl(
        fun(Child, {CX, CY, Acc, FW, FH}) ->
            IsFlex = maps:get(flex, Child, 0) > 0,
            ChildIndex = NumChildren - length(Acc),
            IsLastChild = ChildIndex =:= NumChildren,

            {ChildFlexWidth, NextFW} = if IsFlex andalso Orientation =:= horizontal andalso FW/=[] -> {hd(FW), tl(FW)}; true -> {0, FW} end,
            {ChildFlexHeight, NextFH} = if IsFlex andalso Orientation =:= vertical andalso FH/=[] -> {hd(FH), tl(FH)}; true -> {0, FH} end,

            BaseChildWidth = if
                Orientation =:= vertical -> Width;
                IsFlex -> ChildFlexWidth;
                true -> maps:get(width, Child, Width)
            end,
            BaseChildHeight = if
                Orientation =:= horizontal -> Height;
                IsFlex -> ChildFlexHeight;
                true -> maps:get(height, Child, Height)
            end,

            ChildWidth =
                    case {Orientation, IsLastChild} of
                        {horizontal, false}  -> BaseChildWidth + 10 ;
                        {horizontal, true} -> BaseChildWidth + 10;
                        _ -> BaseChildWidth  % or some other value
                    end,

            ChildHeight =
                    case {Orientation, IsLastChild} of
                        {vertical, false }  -> BaseChildHeight + 10;
                        _ -> BaseChildHeight  + 10 % or some other value 
                    end,

            UpdatedChild = update_child(Child, CX, CY, ChildWidth -1, ChildHeight),

            FinalChild = case maps:get(type, UpdatedChild, widget) of
                container ->
                    calculate_layout(UpdatedChild, CX, CY, ChildWidth, ChildHeight);
                _ ->
                    UpdatedChild
            end,

            NextX = if Orientation =:= horizontal -> CX + ChildWidth; true -> CX end,
            NextY = if Orientation =:= vertical -> CY + ChildHeight; true -> CY end,

            {NextX, NextY, [FinalChild | Acc], NextFW, NextFH}
        end,
        {X, Y, [], FlexWidths, FlexHeights},
        Children
    ),
    lists:reverse(UpdatedChildren).

-doc "Updates a child's position and dimensions.".
-spec update_child(map(), integer(), integer(), integer(), integer()) -> map().
update_child(Child, CX, CY, ChildWidth, ChildHeight) ->
    maps:merge(Child, #{x => CX, y => CY, width => ChildWidth, height => ChildHeight}).
