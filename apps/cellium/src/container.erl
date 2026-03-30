%%% @doc Container widget module for laying out child widgets.
%%%
%%% Containers are special widgets that can hold and arrange other widgets.
%%% They support horizontal and vertical layout orientations and can optionally
%%% display debug visual aids to show their bounds.
%%% @end
-module(container).

-export([render/2, new/2, render_focused/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

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
-spec render(map(), map()) -> map().
render(Container, Buffer) ->
    case maps:get(debug, Container, false) of
        true ->
            #{x := X1, y := Y1, fg := Fg, bg := Bg} = get_common_props(Container),
            X2 = maps:get(width, Container, 0) + X1,
            Y2 = maps:get(height, Container, 0) + Y1,

            lists:foldl(fun(Y, AccY) ->
                lists:foldl(fun(X, AccX) ->
                    cellium_buffer:set_cell(X, Y, $_, Fg, Bg, AccX)
                end, AccY, lists:seq(X1, X2 - 1))
            end, Buffer, lists:seq(Y1, Y2 - 1));
        _Anything ->
            Buffer
    end.

-spec render_focused(map(), map()) -> map().
render_focused(Container, Buffer) ->
    render(Container, Buffer).
