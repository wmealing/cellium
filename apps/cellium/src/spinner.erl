%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(spinner).
-export([render/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => spinner,
                    frame => 0,
                    frames => ["⣷", "⣯", "⣟", "⡿", "⢿", "⣻","⣽","⣾"],
                    type => widget}.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    FrameIdx = maps:get(frame, Widget, 0),
    Frames = maps:get(frames, Widget, ["|", "/", "-", "\\"]),
    Frame = lists:nth((FrameIdx rem length(Frames)) + 1, Frames),
    cellium_buffer:put_string(X, Y, Fg, Bg, Frame, Buffer).
