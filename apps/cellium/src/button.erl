-module(button).

-compile(export_all). 

new(Text, Callback) ->
    (widget:new())#{label => Text,
                    widget_type => button,
                    callback => Callback}.
render(Button) ->
    X1 = maps:get(x, Button),
    X2 = X1 + maps:get(width, Button, 0),
    Y1 = maps:get(y, Button, 0),
    Y2 = Y1 + maps:get(height, Button, 0),
    Label = maps:get(label, Button, <<"NO TEXT">>),

    ButtonLength = X2 - X1,
    WordLength = string:length(Label),
    XOffset = trunc ((ButtonLength / 2) - (WordLength / 2) ),
    YOffset = trunc( (Y2 - Y1) / 2) -1 ,

    text:draw(X1 + XOffset , Y1 + YOffset, X2, Y2, Label),
    box:draw_box(X1, Y1, X2, Y2).
