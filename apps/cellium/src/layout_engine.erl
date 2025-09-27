-module(layout_engine).
-export([calculate_layout/1]).

%% @doc Recursively realizes the layout of a container by calculating the x, y, width,
%% and height for all its children.
-spec calculate_layout(map()) -> map().
calculate_layout(Container) ->
    Children = maps:get(children, Container, []),
    Orientation = maps:get(orientation, Container),
    X = maps:get(x, Container),
    Y = maps:get(y, Container),
    Width = maps:get(width, Container),
    Height = maps:get(height, Container),

    if
        Children == [] ->
            Container;
        true ->
            % Calculate fixed and expand children
            % Note: expand takes priority over size
            ExpandChildren = [Child || Child <- Children, maps:is_key(expand, Child)],
            FixedChildren = [Child || Child <- Children, 
                           maps:is_key(size, Child) andalso not maps:is_key(expand, Child)],

            % Calculate available space based on orientation, accounting for gaps
            NumGaps = length(Children) - 1,
            case Orientation of
                horizontal ->
                    ParentSize = Width,
                    FixedSizeSum = lists:sum([maps:get(size, Child) || Child <- FixedChildren]),
                    AvailableSpace = ParentSize - FixedSizeSum - NumGaps;
                vertical ->
                    ParentSize = Height,
                    FixedSizeSum = lists:sum([maps:get(size, Child) || Child <- FixedChildren]),
                    AvailableSpace = ParentSize - FixedSizeSum - NumGaps
            end,

            ExpandCount = length(ExpandChildren),

            % Distribute available space among expanding children
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

            % Merge fixed and updated expand children lists, preserving original order
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

            % Assign final coordinates and dimensions, including gaps
            {RealizedChildren, _} =
                lists:foldl(fun({Index, Child}, {Acc, CurrentOffset}) ->
                    ChildSize = maps:get(size, Child, 0),
                    Gap = if Index == 0 -> 0; true -> 1 end,

                    RealizedChild = case Orientation of
                        horizontal ->
                            Child#{x => X + CurrentOffset + Gap, y => Y, width => ChildSize, height => Height};
                        vertical ->
                            Child#{x => X, y => Y + CurrentOffset + Gap, width => Width, height => ChildSize}
                    end,
                    % Recursively realize child containers
                    RealizedChildContainer = 
                        case maps:get(type, RealizedChild) of
                            container ->
                                calculate_layout(RealizedChild);
                            widget ->
                                RealizedChild
                        end,

                    {Acc ++ [RealizedChildContainer], CurrentOffset + Gap + ChildSize}
                end, {[], 0}, lists:zip(lists:seq(0, length(UpdatedChildren) - 1), UpdatedChildren)),

            Container#{children => RealizedChildren}
    end.


