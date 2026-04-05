%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Test widget for box_styles:render_box/10
-module(custom_box).

-export([render/2, new/1, new/3]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, 0, 0).

-spec new(term(), non_neg_integer(), non_neg_integer()) -> map().
new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => custom_box,
                    width => Width,
                    height => Height,
                    title => "",
                    title_align => left,
                    style => square,
                    type => widget}.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),
    Title = maps:get(title, Widget, ""),
    TitleAlign = maps:get(title_align, Widget, left),
    StyleName = maps:get(style, Widget, square),

    % Get the box style
    BoxStyle = case StyleName of
        square -> box_styles:square();
        double -> box_styles:double();
        rounded -> box_styles:rounded();
        heavy -> box_styles:heavy();
        minimal -> box_styles:minimal();
        _ -> box_styles:square()
    end,

    if
        Height > 0 andalso Width > 0 ->
            box_styles:render_box(X, Y, Width, Height, BoxStyle, Title, TitleAlign, Fg, Bg, Buffer);
        true ->
            Buffer
    end.
