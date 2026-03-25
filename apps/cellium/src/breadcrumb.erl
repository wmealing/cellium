-module(breadcrumb).
-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => breadcrumb,
                    paths => [],
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := _X, y := _Y, fg := _Fg, bg := _Bg} = get_common_props(Widget),
    %% Breadcrumb rendering logic
    ok.
