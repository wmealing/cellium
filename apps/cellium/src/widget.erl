%%% @doc Base widget module providing common widget functionality.
%%%
%%% This module defines the fundamental widget structure and common operations
%%% used by all widget types in Cellium. Every widget is represented as a map
%%% with specific required and optional keys.
%%% @end
-module(widget).

-export([new/0, create/1, get_common_props/1, color_to_int/1]).

-include("cellium.hrl").
-import(focus_manager, [register_widget/1]).

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

%%% @doc Creates a new base widget map with default values.
%%%
%%% Creates a widget map containing the minimum required fields. This is
%%% typically called by specific widget constructors who then add their
%%% own fields to customize the widget.
%%%
%%% Default values:
%%% - `type': widget
%%% - `widget_type': override_me (should be set by specific widget)
%%% - `color': white
%%% - `padding': all sides set to 0
%%% - `id': override_me (should be set by specific widget)
%%% - `focusable': false
%%%
%%% @returns A base widget map with default values
%%% @end
-spec new() -> map().
new() ->
      #{type => widget,
        widget_type  => override_me,
	color => white, 
        padding => #{top => 0, bottom => 0, left => 0, right => 0},
        id => override_me,
        focusable => false
      }.

%%% @doc Creates and registers a widget with the focus manager if it is focusable.
%%% @param WidgetMap The widget map to create and register.
%%% @returns The widget map.
%%% @end
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

%%% @doc Extracts common rendering properties from a widget map.
%%%
%%% Retrieves the position and color properties needed for rendering,
%%% applying default values when properties are not explicitly set.
%%% This provides a consistent interface for all widgets to access
%%% their rendering properties.
%%%
%%% Default values:
%%% - `x': 0
%%% - `y': 0
%%% - `background-color': value of `?DEFAULT_BG_COLOR' macro
%%% - `color': value of `?DEFAULT_FG_COLOR' macro
%%%
%%% @param Widget A widget map that may contain x, y, color, and background-color keys
%%% @returns A map with keys x, y, fg (foreground), and bg (background)
%%% @end
-spec get_common_props(Widget :: map()) -> #{x := integer(), y := integer(), fg := integer() | atom(), bg := integer() | atom()}.
get_common_props(Widget) ->
    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),
    BgAtom = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    FgAtom = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    #{x => X, y => Y, fg => color_to_int(FgAtom), bg => color_to_int(BgAtom)}.
