-module(container).


-export([render/1, new/2]).

-include("cellium.hrl").


new(Id, Orientation) ->
    (widget:new())#{orientation => Orientation,
                    id => Id,
                    widget_type => non_visible_container,
                    type => container }.

render(Container) ->
    case maps:get(debug, Container, false) of
        true ->
            X1 = maps:get(x, Container, 0),
            X2 = maps:get(width, Container, 0) + X1,
            Y1 = maps:get(y, Container, 0),
            Y2 = maps:get(height, Container, 0) + Y1,
            [?TERMBOX:tb_set_cell(X, Y, $_, 1, 2) || X <- lists:seq(X1, X2),
                                                     Y <- lists:seq(Y1, Y2)];
        _Anything ->
            hi
    end.
