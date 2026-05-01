-module(layout).
-moduledoc """
  The layout module is responsible for calculating the positions and dimensions
  of all widgets in the tree.

  It supports:
  - **Absolute Positioning**: Widgets can be placed at specific coordinates.
  - **Relative Positioning**: Widgets are laid out according to their container's orientation (vertical/horizontal).
  - **Centered Positioning**: Widgets can be automatically centered on the screen.
  - **Flexible Sizing**: Widgets can have a fixed size or expand to fill available space.
  - **Padding**: Containers can define internal padding that affects child placement.
""".

-export([calculate_layout/1, calculate_layout/3]).

-include("cellium.hrl").

-doc """
  Calculates the layout for a widget tree starting from the root, given specific
  target dimensions.

  Parameters:
  - `Widget`: The root widget of the tree to lay out.
  - `Width`: The total available width for the layout.
  - `Height`: The total available height for the layout.
""".
-spec calculate_layout(map(), integer(), integer()) -> map().
calculate_layout(Widget, Width, Height) ->
    NewWidget = Widget#{width => Width,
                        height => Height,
                        parent_width => Width,
                        parent_height => Height},
    calculate_layout(NewWidget).

-doc """
  Recursively calculates the layout of a widget and its children.
  This function is called internally by `calculate_layout/3` and during
  the recursive traversal of the tree.
""".
-spec calculate_layout(map()) -> map().
calculate_layout(Widget) ->
    FocusedWidgetId = get_focused_id(),
    do_calculate_layout(apply_focus(Widget, FocusedWidgetId), FocusedWidgetId).

do_calculate_layout(#{position := absolute} = Widget, FocusedWidgetId) ->
    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),
    layout_container(Widget#{parent_width => Width, parent_height => Height}, FocusedWidgetId);

do_calculate_layout(#{position := centered} = Widget, FocusedWidgetId) ->
    W_Total = ?TERMINAL:term_width(),
    H_Total = ?TERMINAL:term_height(),
    Width = maps:get(requested_width, Widget, maps:get(width, Widget, 40)),
    Height = maps:get(requested_height, Widget, maps:get(height, Widget, maps:get(size, Widget, 10))),
    X = max(0, (W_Total - Width) div 2),
    Y = max(0, (H_Total - Height) div 2),
    CenteredWidget = Widget#{x => X, y => Y, width => Width, height => Height, 
                            position => absolute, parent_width => Width, parent_height => Height},
    layout_container(CenteredWidget, FocusedWidgetId);

do_calculate_layout(Widget, FocusedWidgetId) ->
    layout_container(Widget, FocusedWidgetId).

layout_container(Container, FocusedWidgetId) ->
    Children = maps:get(children, Container, []),
    WidgetType = maps:get(widget_type, Container, undefined),
    
    Padding = get_padding(Container),
    #{top := PT, right := PR, bottom := PB, left := PL} = Padding,

    X = maps:get(x, Container, 0),
    Y = maps:get(y, Container, 0),
    
    CWidth = maps:get(width, Container, maps:get(parent_width, Container, ?TERMINAL:term_width())),
    CHeight = maps:get(height, Container, maps:get(parent_height, Container, ?TERMINAL:term_height())),

    InnerWidth = max(0, CWidth - PL - PR),
    InnerHeight = max(0, CHeight - PT - PB),

    if Children == [] ->

            Container;
       true ->
            InnerX = X + PL,
            InnerY = Y + PT,

            RealizedChildren = 
                if WidgetType == tab orelse WidgetType == dialog ->
                    layout_stacked_children(Children, InnerX, InnerY, InnerWidth, InnerHeight, FocusedWidgetId);
                true ->
                    Orientation = maps:get(orientation, Container, horizontal),
                    layout_flex_children(Children, InnerX, InnerY, InnerWidth, InnerHeight, Orientation, FocusedWidgetId)
                end,
            Container#{children => RealizedChildren}
    end.

layout_stacked_children(Children, X, Y, W, H, FocusedWidgetId) ->
    [ realize_child(Child#{x => X, y => Y, width => W, height => H,
                           parent_width => W, parent_height => H}, FocusedWidgetId) 
      || Child <- Children ].

layout_flex_children(Children, X, Y, W, H, Orientation, FocusedWidgetId) ->
    UpdatedChildren = calculate_expand_sizes(Children, W, H, Orientation),
    {RealizedChildren, _} =
        lists:foldl(fun(Child, {Acc, Offset}) ->
            IsAbsolute = maps:get(position, Child, relative) =:= absolute 
                         orelse maps:get(position, Child, relative) =:= centered,
            Size = maps:get(size, Child, 0),
            
            RealizedChild = if IsAbsolute -> Child;
                               true -> position_child(Child, X, Y, W, H, Orientation, Size, Offset)
                            end,
            
            FinalChild = realize_child(RealizedChild, FocusedWidgetId),
            NewOffset = if IsAbsolute -> Offset; true -> Offset + Size end,
            {[FinalChild | Acc], NewOffset}
        end, {[], 0}, UpdatedChildren),
    lists:reverse(RealizedChildren).

position_child(Child, X, Y, _W, H, horizontal, Size, Offset) ->
    Child#{x => X + Offset, y => Y, width => Size, height => H, parent_width => Size, parent_height => H};
position_child(Child, X, Y, W, _H, vertical, Size, Offset) ->
    Child#{x => X, y => Y + Offset, width => W, height => Size, parent_width => W, parent_height => Size}.

realize_child(#{type := container} = Child, FocusedWidgetId) ->
    do_calculate_layout(apply_focus(Child, FocusedWidgetId), FocusedWidgetId);
realize_child(#{type := widget} = Child, FocusedWidgetId) ->
    apply_focus(Child, FocusedWidgetId).

calculate_expand_sizes(Children, W, H, Orientation) ->
    ExpandCount = length([C || C <- Children, maps:is_key(expand, C)]),
    if ExpandCount == 0 -> Children;
       true ->
            ParentSize = if Orientation == horizontal -> W; true -> H end,
            FixedSizeSum = lists:sum([maps:get(size, C, 0) || C <- Children, not maps:is_key(expand, C)]),
            AvailableSpace = max(0, ParentSize - FixedSizeSum),
            BaseSize = AvailableSpace div ExpandCount,
            Remainder = AvailableSpace rem ExpandCount,
            {Res, _} = lists:mapfoldl(fun(C, Rem) ->
                case maps:is_key(expand, C) of
                    true -> 
                        Size = if Rem > 0 -> BaseSize + 1; true -> BaseSize end,
                        {C#{size => Size}, max(0, Rem - 1)};
                    false -> {C, Rem}
                end
            end, Remainder, Children),
            Res
    end.

apply_focus(Widget, FocusedWidgetId) ->
    Id = maps:get(id, Widget, undefined),
    IsFocused = (Id =/= undefined) andalso (Id =:= FocusedWidgetId),
    Widget#{focused => IsFocused, has_focus => IsFocused}.

get_focused_id() ->
    case whereis(focus_manager) of
        undefined -> undefined;
        _ -> 
            case focus_manager:get_focused() of
                {ok, FW} -> FW;
                _ -> undefined
            end
    end.

get_padding(Widget) ->
    Default = case maps:get(widget_type, Widget, undefined) of
        frame -> #{top => 1, right => 1, bottom => 1, left => 1};
        dialog -> #{top => 1, right => 1, bottom => 1, left => 1};
        box -> #{top => 1, right => 1, bottom => 1, left => 1};
        _ -> #{top => 0, right => 0, bottom => 0, left => 0}
    end,
    Padding = maps:get(padding, Widget, Default),
    #{top => maps:get(top, Padding, 0),
      right => maps:get(right, Padding, 0),
      bottom => maps:get(bottom, Padding, 0),
      left => maps:get(left, Padding, 0)}.

