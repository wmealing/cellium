-module(spinner).
-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => spinner,
                    frame => 0,
                    frames => ["|", "/", "-", "\\"],
                    type => widget}.

-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    FrameIdx = maps:get(frame, Widget, 0),
    Frames = maps:get(frames, Widget, ["|", "/", "-", "\\"]),
    Frame = lists:nth((FrameIdx rem length(Frames)) + 1, Frames),
    ?TERMBOX:tb_print(X, Y, Fg, Bg, Frame),
    ok.
