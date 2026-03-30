-module(tab).
-export([render/2, new/1]).

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

-spec render(map(), map()) -> map().
render(_Widget, Buffer) ->
    %% Tab rendering logic
    Buffer.
