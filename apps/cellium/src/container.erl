%%% @doc Container widget module for laying out child widgets.
%%%
%%% Containers are special widgets that can hold and arrange other widgets.
%%% They support horizontal and vertical layout orientations and can optionally
%%% display debug visual aids to show their bounds.
%%% @end
-module(container).

-export([render/1, new/2]).

-include("cellium.hrl").

%%% @doc Creates a new container widget.
%%%
%%% Creates a container that can hold child widgets arranged in the specified
%%% orientation. By default, containers have no padding.
%%%
%%% @param Id Unique identifier for the container
%%% @param Orientation Layout direction: `horizontal' or `vertical'
%%% @returns A container widget map
%%% @end
-spec new(term(), horizontal | vertical) -> map().
new(Id, Orientation) ->
    (widget:new())#{orientation => Orientation,
                    padding => #{top => 0, bottom => 0, left => 0, right => 0},
                    id => Id,
                    widget_type => container,
                    type => container }.

%%% @doc Renders the container and its debug visualization if enabled.
%%%
%%% Normally renders nothing visible. When debug mode is enabled, draws
%%% an underscore pattern filling the container's bounds to visualize
%%% its layout area.
%%%
%%% @param Container The container widget map
%%% @returns ok or hi (internal token)
%%% @end
-spec render(map()) -> ok | hi.
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
