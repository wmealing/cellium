-module(select).
-export([render/1, new/1]).

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

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    %% Rendering logic for dropdown/select here
    ?TERMBOX:tb_print(X, Y, Fg, Bg, "[ Select... ]"),
    ok.
