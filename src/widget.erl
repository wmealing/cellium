-module(widget).
-moduledoc """
Base widget module providing common widget functionality.

This module defines the fundamental widget structure and common operations
used by all widget types in Cellium. Every widget is represented as a map
with specific required and optional keys.

## Widget Structure

All widgets are maps containing:
- `type`: Either `widget` or `container`
- `widget_type`: Specific widget type (text, button, checkbox, etc.)
- `id`: Unique identifier
- `color`: Foreground color (default: white)
- `background-color`: Background color (default: from ?DEFAULT_BG_COLOR)
- `padding`: Padding map with top, bottom, left, right
- `focusable`: Whether the widget can receive focus

## Common Properties

### Layout Properties
- `x`, `y`: Position (set by layout system)
- `width`, `height`: Dimensions (set by layout system)
- `size`: Fixed size in layout direction
- `expand`: Request to fill available space

### Visual Properties
- `color`: Foreground color atom (black, red, green, yellow, blue, magenta, cyan, white)
- `background-color`: Background color atom
- Bright variants: `bright_black`, `bright_red`, etc.

### Focus Properties
- `focusable`: Can this widget receive focus?
- `focused`: Is this widget currently focused? (set by layout)
- `has_focus`: Does this widget or a child have focus? (set by layout)

## Creating Widgets

Specific widget modules call `widget:new()` and add their own properties:
```
button:new(my_button, "Click Me")
% Calls widget:new() internally and adds button-specific fields
```
""".

-export([new/0, create/1, destroy/1, destroy_tree/1, get_common_props/1, color_to_int/1]).

-include("cellium.hrl").
-import(focus_manager, [register_widget/1]).

-doc """
Converts color atoms to their integer or atom representation.

Accepts color atoms (black, red, green, yellow, blue, magenta, cyan, white)
and their bright variants, RGB tuples, or integers. Returns the color value
for use in rendering.
""".
-spec color_to_int(Color :: atom() | integer()) -> integer() | atom().
color_to_int(Color) when is_integer(Color) ->
    Color;
color_to_int({R,G,B}) -> {R,G,B};
color_to_int(default) -> default;
color_to_int(black) -> black;
color_to_int(red) -> red;
color_to_int(green) -> green;
color_to_int(yellow) -> yellow;
color_to_int(blue) -> blue;
color_to_int(magenta) -> magenta;
color_to_int(cyan) -> cyan;
color_to_int(white) -> white;
color_to_int(bright_black) -> bright_black;
color_to_int(bright_red) -> bright_red;
color_to_int(bright_green) -> bright_green;
color_to_int(bright_yellow) -> bright_yellow;
color_to_int(bright_blue) -> bright_blue;
color_to_int(bright_magenta) -> bright_magenta;
color_to_int(bright_cyan) -> bright_cyan;
color_to_int(bright_white) -> bright_white;
color_to_int(_) -> default.

-doc """
Creates a new base widget map with default values.

This is typically called by specific widget constructors who then add their
own fields to customize the widget.

Default values:
- `type`: widget
- `widget_type`: override_me (should be set by specific widget)
- `color`: white
- `padding`: all sides set to 0
- `id`: override_me (should be set by specific widget)
- `focusable`: false
""".
-spec new() -> map().
new() ->
      #{type => widget,
        widget_type  => override_me,
	color => white,
        padding => #{top => 0, bottom => 0, left => 0, right => 0},
        id => override_me,
        focusable => false
      }.

-doc """
Creates and registers a widget with the focus manager if it is focusable.

If the widget has `focusable => true`, registers it with the focus manager
for keyboard navigation support.
""".
-spec create(map()) -> map().
create(WidgetMap) ->
    case maps:get(focusable, WidgetMap, false) of
        true ->
            case maps:get(id, WidgetMap, undefined) of
                undefined ->
                    logger:warning("Cannot register widget with no ID: ~p", [WidgetMap]);
                Id ->
                    focus_manager:register_widget(Id)
            end;
        false ->
            ok
    end,
    WidgetMap.

-doc """
Destroys a widget and unregisters it from the focus manager if it is focusable.

This function should be called when a widget is being removed from the UI
to ensure it is properly cleaned up. If the widget has `focusable => true`,
it will be unregistered from the focus manager.

This is the counterpart to `create/1` and should be called when transitioning
between screens or removing widgets from the widget tree.
""".
-spec destroy(map()) -> ok.
destroy(WidgetMap) ->
    case maps:get(focusable, WidgetMap, false) of
        true ->
            case maps:get(id, WidgetMap, undefined) of
                undefined ->
                    ok;
                Id ->
                    focus_manager:unregister_widget(Id)
            end;
        false ->
            ok
    end,
    ok.

-doc """
Destroys a widget tree recursively, including all children.

This function destroys a widget and all its children (if it's a container).
It walks the widget tree depth-first, destroying children before parents.
Each focusable widget will be unregistered from the focus manager.

Use this when removing an entire screen or container hierarchy.
""".
-spec destroy_tree(map()) -> ok.
destroy_tree(WidgetMap) ->
    % First destroy all children if this is a container
    case maps:get(children, WidgetMap, undefined) of
        undefined -> ok;
        Children when is_list(Children) ->
            lists:foreach(fun destroy_tree/1, Children);
        _ -> ok
    end,
    % Then destroy this widget
    destroy(WidgetMap).

-spec brighten(atom()) -> atom().
brighten(black) -> bright_black;
brighten(red) -> bright_red;
brighten(green) -> bright_green;
brighten(yellow) -> bright_yellow;
brighten(blue) -> bright_blue;
brighten(magenta) -> bright_magenta;
brighten(cyan) -> bright_cyan;
brighten(white) -> bright_white;
brighten(Color) -> Color.

-doc """
Extracts common rendering properties from a widget map.

Retrieves the position and color properties needed for rendering,
applying default values when properties are not explicitly set.
This provides a consistent interface for all widgets to access
their rendering properties.

Default values:
- `x`: 0
- `y`: 0
- `background-color`: value of `?DEFAULT_BG_COLOR` macro
- `color`: value of `?DEFAULT_FG_COLOR` macro

Returns a map with keys `x`, `y`, `fg` (foreground), and `bg` (background).
""".
-spec get_common_props(Widget :: map()) -> #{x := integer(), y := integer(), fg := integer() | atom(), bg := integer() | atom()}.
get_common_props(Widget) ->
    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),
    BgAtom = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    FgAtom = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    #{x => X, y => Y, fg => color_to_int(FgAtom), bg => color_to_int(BgAtom)}.
