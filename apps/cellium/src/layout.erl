-module(layout).
-export([calculate_layout/1, calculate_layout/3]).

-include("cellium.hrl").

calculate_layout(Widget, Width, Height) ->
    NewWidget = Widget#{width => Width,
                        height => Height},
    calculate_layout(NewWidget).

%% @doc Recursively realizes the layout of a container by calculating the x, y, width,
%% and height for all its children.

-spec calculate_layout(map()) -> map().
calculate_layout(#{position := absolute} = Widget) ->
    logger:debug("IGNORING LAYOUT FOR WIDGET ~p", [Widget]),
    Widget;

calculate_layout(Container) ->

    Children = maps:get(children, Container, []), 
    Orientation = maps:get(orientation, Container, horizontal),

    DefaultPadding = case maps:get(widget_type, Container, undefined) of
        frame -> #{top => 1, right => 1, bottom => 1, left => 1};
        _ -> #{top => 0, right => 0, bottom => 0, left => 0}
    end,

    Padding = maps:get(padding, Container, DefaultPadding),
    PaddingTop = maps:get(top, Padding, 0),
    PaddingRight = maps:get(right, Padding, 0),
    PaddingBottom = maps:get(bottom, Padding, 0),
    PaddingLeft = maps:get(left, Padding, 0),

    X = maps:get(x, Container, 0) + PaddingLeft,
    Y = maps:get(y, Container, 0) + PaddingTop,
    Width = max(0, maps:get(width, Container, ?TERMBOX:tb_width()) - PaddingLeft - PaddingRight),
    Height = max(0, maps:get(height, Container, ?TERMBOX:tb_height()) - PaddingTop - PaddingBottom),


    if Children == [] ->
            Container;
       true ->
            % Calculate fixed and expand children
            % Note: expand takes priority over size
            ExpandChildren = [Child || Child <- Children, maps:is_key(expand, Child)],
            FixedChildren = [Child || Child <- Children, 
                           maps:is_key(size, Child) andalso not maps:is_key(expand, Child)],

            % Calculate available space based on orientation
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
                    % Check if child has absolute positioning
                    IsAbsolute = maps:get(position, Child, relative) =:= absolute,
                    
                    ChildSize = maps:get(size, Child, 0),
                    Gap = 0,

                    RealizedChild = case IsAbsolute of
                        true ->
                            % Keep original x,y for absolute positioned widgets
                            Child;
                        false ->
                            % Apply layout positioning for relative widgets
                            case Orientation of
                                horizontal ->
                                    Child#{x => X + CurrentOffset + Gap, y => Y, width => ChildSize, height => Height};
                                vertical ->
                                    Child#{x => X, y => Y + CurrentOffset + Gap, width => Width, height => ChildSize}
                            end
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
