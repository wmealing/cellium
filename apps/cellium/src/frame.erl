-module(frame).

-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => visible_container,
                    padding => #{top => 1, bottom => 1, left => 1, right => 1},
                    type => container }.

render(Widget) ->

    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    FrameTitle = maps:get(text, Widget, <<"Untitled">>),


    [?TERMBOX:tb_set_cell(X1, Y1, $_, 1, 2) || X1 <- lists:seq(X, X + Width),
                                               Y1 <- lists:seq(Y, Y + Height)],

    Box = box_styles:double(),

    % okay so now we draw that table
    table:draw_table(X,Y, Height, Fg,Bg, Box, [Width -1]),
    ?TERMBOX:tb_print(X + 2, Y, Fg, Bg, FrameTitle),
    ok.
