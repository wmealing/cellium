-module(layout_engine).
-export([calculate_layout/1]).


%% @doc Calculates the bounding box for each child widget within a container.
%%      It implements a basic flexbox-like model for 'row' or 'column' layouts.
%% -spec calculate_layout(Container) -> list().
%%calculate_layout(Container, #bbox{width = CWidth, height = CHeight} = Bbox) ->
calculate_layout(Container) ->

    Direction = maps:get(direction, Container, row),
    Children = maps:get(children, Container),
    TotalFlex = lists:sum([maps:get(flex, Child, 0) || Child <- Children]),

    case Direction of
        row ->
            %% Horizontal layout
            FixedSize = lists:sum([maps:get(size, Child, 0) || Child <- Children]),
            AvailableSpace = maps:get(width, Container) - FixedSize,
            CurrentX = maps:get(x, Container) - FixedSize,
            lists:map(fun(Child) ->
                Flex = maps:get(flex, Child, 0),
                FlexWidth = if TotalFlex > 0 -> trunc(AvailableSpace * (Flex / TotalFlex));
                              true -> 0
                          end,
                Width = maps:get(size, Child, 0) + FlexWidth,
                %%ChildBbox = %% Bbox#bbox{x = CurrentX, width = Width},
                Child#{x => CurrentX, width => Width}
            end, Children);

        _ ->
             erlang:error(not_implemented)
    end.


get_first([Head|Tail]) ->
    Head;
get_first([]) ->
    nil. % Handle empty list case if desired
