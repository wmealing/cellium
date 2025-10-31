-module(button).

-export([render/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

new(Text, Callback) ->
    (widget:new())#{label => Text,
                    widget_type => button,
                    callback => Callback}.

render(Widget) ->
    #{x := X1, y := Y1, fg := Fg, bg := Bg} = get_common_props(Widget),

    X2 = X1 + maps:get(width, Widget, 0),
    Y2 = Y1 + maps:get(height, Widget, 0),
    Label = maps:get(label, Widget, [<<"NO TEXT">>]),

    ButtonLength = X2 - X1,
    WordLength = string:length(Label),
    XOffset = trunc ((ButtonLength / 2) - (WordLength / 2) ),
    YOffset = trunc( (Y2 - Y1) / 2) -1 ,

    text:draw_words(X1 + XOffset , Y1 + YOffset, X2, Y2, Bg, Fg , Label),
    table:draw_box(X1, Y1, X2, Y2, Fg, Bg).


