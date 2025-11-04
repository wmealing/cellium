%%% @doc Base widget module providing common widget functionality.
%%%
%%% This module defines the fundamental widget structure and common operations
%%% used by all widget types in Cellium. Every widget is represented as a map
%%% with specific required and optional keys.
%%% @end
-module(widget).

-export([new/0, get_common_props/1]).

-include("cellium.hrl").

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
%%%
%%% @returns A base widget map with default values
%%% @end
-spec new() -> map().
new() ->
      #{type => widget,
        widget_type  => override_me,
	color => white, 
        padding => #{top => 0, bottom => 0, left => 0, right => 0},
        id => override_me  }.

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
-spec get_common_props(Widget :: map()) -> #{x := integer(), y := integer(), fg := atom(), bg := atom()}.
get_common_props(Widget) ->
    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),
    #{x => X, y => Y, fg => Fg, bg => Bg}.
