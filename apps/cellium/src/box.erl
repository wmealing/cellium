%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(box).

% API
-export([render/1, new/3]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => box,
                    width => Width,
                    height => Height,
                    type => widget }.

render(Widget) ->
    HasFocus = maps:get(has_focus, Widget, false),

    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    BoxStyle =
        case HasFocus of
            true ->
                box_styles:double();
            false ->
                box_styles:square()
        end,

    Height = maps:get(height,Widget, 0),
    Width = maps:get(width, Widget, 0),

    table:draw_table(X, Y, Height - 1, Fg,Bg, BoxStyle, [Width - 2]),
    ok.
