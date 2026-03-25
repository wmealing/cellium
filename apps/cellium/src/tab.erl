-module(tab).
-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => tab,
                    tabs => [],
                    active_tab => 0,
                    focusable => true,
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := _X, y := _Y, fg := _Fg, bg := _Bg} = get_common_props(Widget),
    %% Tab rendering logic
    ok.
