%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(box).

% API
-export([render/1, new/3, draw_box/6]).

-include("cellium.hrl").

new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => box,
                    width => Width,
                    height => Height,
                    type => widget }.

render(Widget) ->
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    X1 = maps:get(x, Widget),
    X2 = X1 + maps:get(width, Widget, 0),
    Y1 = maps:get(y, Widget, 0),
    Y2 = Y1 + maps:get(height, Widget, 0),
    draw_box(X1, Y1, X2, Y2, Bg, Fg).

draw_box(X1, Y1, X2, Y2, Bg, Fg) ->
    box_styles:draw_vertical_line(Y1, X2, Y2, Bg, Fg),
    box_styles:draw_horizontal_line(X1 + 1, Y1, X2, Bg, Fg),
    box_styles:draw_vertical_line(Y1, X1, Y2, Bg, Fg),
    box_styles:draw_horizontal_line(X1 + 1, Y2, X2, Bg, Fg),
    ?TERMBOX:tb_set_cell(X1, Y1, $┌, Bg, Fg),
    ?TERMBOX:tb_set_cell(X2, Y1, $┐, Bg, Fg),
    ?TERMBOX:tb_set_cell(X1, Y2, $└, Bg, Fg),
    ?TERMBOX:tb_set_cell(X2, Y2, $┘, Bg, Fg).
