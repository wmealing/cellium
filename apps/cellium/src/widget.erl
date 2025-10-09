-module(widget).

-export([new/0]).

%% Create a new widget.
new() ->
      #{type => widget,
        widget_type  => override_me,
        id => override_me  }.
