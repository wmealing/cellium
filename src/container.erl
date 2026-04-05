-module(container).
-moduledoc """
  Container widgets are used to arrange other widgets in a specific layout.
  They support both horizontal and vertical orientations and handle the
  distribution of space among their children.
""".

-export([render/2, new/2, render_focused/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc """
  Creates a new container widget.

  Parameters:
  - `Id`: A unique identifier for the container.
  - `Orientation`: The layout direction, either `horizontal` or `vertical`.
""".
-spec new(term(), horizontal | vertical) -> map().
new(Id, Orientation) ->
    (widget:new())#{orientation => Orientation,
                    padding => #{top => 0, bottom => 0, left => 0, right => 0},
                    id => Id,
                    widget_type => container,
                    type => container }.

-doc """
  Renders the container and its debug visualization if enabled.

  Normally renders nothing visible. When debug mode is enabled, draws
  an underscore pattern filling the container's bounds to visualize
  its layout area.
""".
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

-doc "Renders the container when it or one of its children has focus.".
-spec render_focused(map(), map()) -> map().
render_focused(Container, Buffer) ->
    render(Container, Buffer).
