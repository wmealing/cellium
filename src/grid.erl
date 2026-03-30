-module(grid).
-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => grid,
                    children => [],
                    rows => 1,
                    cols => 1,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := _X, y := _Y, fg := _Fg, bg := _Bg} = get_common_props(Widget),
    %% Grid rendering logic here
    ok.
