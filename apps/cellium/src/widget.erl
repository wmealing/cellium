-module(widget).

-export([new/0, get_common_props/1]).

-include("cellium.hrl").

%% Create a new widget.
new() ->
      #{type => widget,
        widget_type  => override_me,
	color => white, 
        padding => #{top => 0, bottom => 0, left => 0, right => 0},
        id => override_me  }.

%% @doc Extracts common rendering properties (x, y, fg, bg) from a widget map.
%%      Uses default values if properties are not explicitly set in the widget.
-spec get_common_props(Widget :: map()) -> #{x := integer(), y := integer(), fg := atom(), bg := atom()}.
get_common_props(Widget) ->
    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),
    #{x => X, y => Y, fg => Fg, bg => Bg}.
