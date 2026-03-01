%%% @doc Floating container widget for absolute-positioned layouts.
%%%
%%% This module provides a container that ignores the normal layout flow
%%% and can be positioned at specific coordinates. Useful for overlays,
%%% dialogs, and other UI elements that need precise positioning.
%%% @end
-module(floating_container).

-export([render/1, new/6]).

-include("cellium.hrl").

%%% @doc Creates a new floating container widget.
%%%
%%% Creates a container with absolute positioning that is excluded from
%%% normal layout calculations. The container is positioned at specific
%%% coordinates with fixed dimensions.
%%%
%%% @param Id Unique identifier for the floating container
%%% @param Orientation Layout direction for children: `horizontal' or `vertical'
%%% @param X Absolute X coordinate on screen
%%% @param Y Absolute Y coordinate on screen
%%% @param Width Width of the container in characters
%%% @param Height Height of the container in characters
%%% @returns A container widget map configured for absolute positioning
%%% @end
-spec new(term(), horizontal | vertical, 
          non_neg_integer(), non_neg_integer(), 
          non_neg_integer(), non_neg_integer()) -> map().
new(Id, Orientation, X,Y, Width, Height) ->
    (widget:new())#{orientation => Orientation,
                    layout_ignore => true,
                    padding => #{top => 0, bottom => 0, left => 0, right => 0},
                    id => Id,
                    x => X,
                    width => Width,
                    height => Height,
                    y => Y,
                    widget_type => floating_container,
                    type => container }.

%%% @doc Renders the floating container's debug visualization if enabled.
%%%
%%% Normally renders nothing visible. When debug mode is enabled, draws
%%% an underscore pattern filling the container's bounds to visualize
%%% its layout area.
%%%
%%% @param FloatingContainer The floating container widget map
%%% @returns ok or hi (internal token)
%%% @end
-spec render(map()) -> ok | hi.
render(FloatingContainer) ->
    case maps:get(debug, FloatingContainer, false) of
        true ->
            X1 = maps:get(x, FloatingContainer, 0),
            X2 = maps:get(width, FloatingContainer, 0) + X1,
            Y1 = maps:get(y, FloatingContainer, 0),
            Y2 = maps:get(height, FloatingContainer, 0) + Y1,
            [?TERMBOX:tb_set_cell(X, Y, $_, 1, 2) || X <- lists:seq(X1, X2),
                                                     Y <- lists:seq(Y1, Y2)];
        _Anything ->
            hi
    end.
