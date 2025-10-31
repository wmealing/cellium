-module(checkbox).
-export([new/2, render/1, toggle/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

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
    Checked = maps:get(checked, Widget),
    Label = maps:get(label, Widget),

    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

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
