-module(widget).

-export([new/0]).

%% Create a new widget.
new() ->
      #{type => widget,
        widget_type  => override_me,
        'background-color' => black,
	color => white, 
        padding => #{top => 0, bottom => 0, left => 0, right => 0},
        id => override_me  }.
