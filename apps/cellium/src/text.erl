-module(text).
-export([render/2, new/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    new(Id, "").

-spec new(term(), string()) -> map().
new(Id, Content) ->
    (widget:new())#{id => Id,
                    widget_type => text,
                    text => Content,
                    type => widget}.

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = maps:get(text, Widget, ""),
    Wrap = maps:get(wrap, Widget, false),

    case Wrap of
        true ->
            % Wrap text at widget width (set by layout) and render each line
            Width = maps:get(width, Widget),
            TextBin = list_to_binary(Text),
            Lines = greedy_wrap:word_wrap(TextBin, Width),
            render_lines(X, Y, Fg, Bg, Lines, Buffer);
        false ->
            % No wrapping - render as single line
            cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer)
    end.

render_lines(_X, _Y, _Fg, _Bg, [], Buffer) ->
    Buffer;
render_lines(X, Y, Fg, Bg, [Line | Rest], Buffer) ->
    LineStr = binary_to_list(Line),
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, LineStr, Buffer),
    render_lines(X, Y + 1, Fg, Bg, Rest, NewBuffer).
