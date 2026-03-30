-module(menu).
-export([render/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => menu,
                    items => [],
                    selected_index => 0,
                    focusable => true,
                    type => widget}.

-spec render(map(), map()) -> map().
render(_Widget, Buffer) ->
    %% Menu rendering logic
    Buffer.
