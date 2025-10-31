-module(checkbox).
-export([new/2, render/1, toggle/1]).

-include("cellium.hrl").

-spec new(term(), binary()) -> map().
new(Id, Label) ->
    (widget:new())#{id => Id,
                    type => widget,
                    widget_type => checkbox,
                    label => Label,
                    padding =>
                        #{top => 1,
                          bottom => 1,
                          left => 1,
                          right => 1},
                    checked => false
                    }.

render(Widget) ->
    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),
    Checked = maps:get(checked, Widget),
    Label = maps:get(label, Widget),

    %% what ?
    Bg = maps:get('background-color', Widget, ?DEFAULT_BG_COLOR),
    Fg = maps:get(color, Widget, ?DEFAULT_FG_COLOR),

    case Checked of
        true ->
            ?TERMBOX:tb_print(X,Y, Fg, Bg,<<"[x]">> );
        false ->
            ?TERMBOX:tb_print(X,Y, Fg, Bg, <<"[ ]">> )
    end,

    ?TERMBOX:tb_print(X + 4 ,Y, Fg, Bg, Label).

-spec toggle(map()) -> map().
toggle(Widget = #{}) ->
    NewChecked = not maps:get(checked, Widget),
    Widget#{checked => NewChecked}.
