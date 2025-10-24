%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(box).

% API
-export([render/1, new/3]).

-include("cellium.hrl").

new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => box,
                    width => Width,
                    height => Height,
                    type => widget }.

render(Widget) ->
    Id = maps:get(id, Widget, undefined),
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),
    HasFocus = maps:get(has_focus, Widget, false),

    BoxStyle =
        case HasFocus of
            true ->
                box_styles:double();
            false ->
                box_styles:square()
        end,

    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),

    Height = maps:get(height,Widget, 0),
    Width = maps:get(width, Widget, 0),

    table:draw_table(X, Y, Height, Fg,Bg, BoxStyle, [Width -1]),
    ok.
