-module(button).

-export([render/1, new/2]).

-include("cellium.hrl").

new(Text, Callback) ->
    (widget:new())#{label => Text,
                    widget_type => button,
                    callback => Callback}.
render(Widget) ->
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    X1 = maps:get(x, Widget),
    X2 = X1 + maps:get(width, Widget, 0),
    Y1 = maps:get(y, Widget),
    Y2 = Y1 + maps:get(height, Widget, 0),
    Label = maps:get(label, Widget, <<"NO TEXT">>),

    ButtonLength = X2 - X1,
    WordLength = string:length(Label),
    XOffset = trunc ((ButtonLength / 2) - (WordLength / 2) ),
    YOffset = trunc( (Y2 - Y1) / 2) -1 ,

    text:draw(X1 + XOffset , Y1 + YOffset, X2, Y2, Label),
    box:draw_box(X1, Y1, X2, Y2, Bg, Fg).
