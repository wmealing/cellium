%%%-------------------------------------------------------------------
%%% @doc Layout Engine Module
%%%
%%% This module implements the core layout calculation engine for Cellium.
%%% It transforms a widget tree with relative dimensions and positioning
%%% into an absolute coordinate system ready for rendering.
%%%
%%% The layout engine supports:
%%% - Horizontal and vertical container orientation
%%% - Fixed-size and expanding children
%%% - Padding and spacing
%%% - Absolute and relative positioning
%%% - Nested container layouts
%%%
%%% Layout calculation proceeds recursively from parent containers to their
%%% children, distributing available space according to size constraints and
%%% expand properties.
%%% @end
%%%-------------------------------------------------------------------
-module(layout).
-export([calculate_layout/1, calculate_layout/3]).

-include("cellium.hrl").

%%% @doc Calculates layout with explicit dimensions.
%%%
%%% Wrapper function that sets explicit width and height on a widget
%%% before performing layout calculation. Useful for setting root
%%% container dimensions.
%%%
%%% @param Widget The widget or container to layout
%%% @param Width The width to set
%%% @param Height The height to set
%%% @returns Updated widget map with calculated layout
%%% @end
-spec calculate_layout(map(), integer(), integer()) -> map().
calculate_layout(Widget, Width, Height) ->
    NewWidget = Widget#{width => Width,
                        height => Height},
    calculate_layout(NewWidget).

%%% @doc Recursively calculates layout for a widget tree.
%%%
%%% This is the main layout calculation function that transforms a widget tree
%%% with relative dimensions into absolute coordinates. The function:
%%% 
%%% 1. Handles absolutely-positioned widgets by skipping layout
%%% 2. Applies container padding (frames get default padding)
%%% 3. Separates children into fixed-size and expanding categories
%%% 4. Distributes available space among expanding children
%%% 5. Assigns final coordinates based on container orientation
%%% 6. Recursively processes nested containers
%%%
%%% Children with the expand property share remaining space equally after
%%% accounting for fixed-size children. The last expanding child receives any
%%% remaining pixels from integer division.
%%%
%%% @param Widget Widget or container map to layout
%%% @returns Widget map with calculated x, y, width, height for all descendants
%%% @end

-spec calculate_layout(map()) -> map().
calculate_layout(#{position := absolute} = Widget) ->
    %% Absolutely positioned widgets ignore layout calculation
    Widget;

calculate_layout(OriginalContainer) ->
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
        _ -> #{top => 0, right => 0, bottom => 0, left => 0}
    end,

    Padding = maps:get(padding, Container, DefaultPadding),
    PaddingTop = maps:get(top, Padding, 0),
    PaddingRight = maps:get(right, Padding, 0),
    PaddingBottom = maps:get(bottom, Padding, 0),
    PaddingLeft = maps:get(left, Padding, 0),

    % Core dimension calculation:
    % 1. Use existing width/height (assigned by parent)
    % 2. Fallback to terminal size ONLY if we are truly at the root (no parent assigned dimensions)
    X = maps:get(x, Container, 0),
    Y = maps:get(y, Container, 0),
    
    ContainerWidth = maps:get(width, Container, ?TERMBOX:tb_width()),
    ContainerHeight = maps:get(height, Container, ?TERMBOX:tb_height()),

    Width = max(0, ContainerWidth - PaddingLeft - PaddingRight),
    Height = max(0, ContainerHeight - PaddingTop - PaddingBottom),

%%    logger:debug("Layout: ID=~p, Type=~p, X=~p, Y=~p, W=~p, H=~p (Padding: ~p)", 
%%                 [Id, WidgetType, X, Y, ContainerWidth, ContainerHeight, Padding]),

    if Children == [] ->
            Container;
       true ->
            InnerX = X + PaddingLeft,
            InnerY = Y + PaddingTop,
            
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
                                    Child#{x => InnerX + CurrentOffset + Gap, y => InnerY, width => ChildSize, height => Height};
                                vertical ->
                                    Child#{x => InnerX, y => InnerY + CurrentOffset + Gap, width => Width, height => ChildSize}
                            end
                    end,
                    % Recursively realize child containers
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
    end.
