-module(select).
-export([render/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => select,
                    options => [],
                    selected_index => 0,
                    focusable => true,
                    type => widget}.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    %% Rendering logic for dropdown/select here
    cellium_buffer:put_string(X, Y, Fg, Bg, "[ Select... ]", Buffer).
