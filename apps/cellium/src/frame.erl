%%% @doc Frame widget module for rendering bordered containers with titles.
%%%
%%% Frames are visual containers that display a border around their content area
%%% and can show an optional title. They automatically apply padding to create
%%% space for child widgets inside the border.
%%% @end
-module(frame).

-export([render/1, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%% @doc Creates a new frame widget.
%%%
%%% Creates a bordered container with padding. Frames are special containers
%%% that provide visual separation and can display titles.
%%%
%%% @param Id Unique identifier for the frame
%%% @returns A frame widget map configured as a visible container with padding
%%% @end
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => visible_container,
                    padding => #{top => 1, bottom => 1, left => 1, right => 1},
                    type => container }.

%%% @doc Renders the frame with its border and optional title.
%%%
%%% Draws a double-line bordered box around the frame's bounds and displays
%%% the frame's title (if set) in the top border. The title defaults to
%%% "Untitled" if not specified.
%%%
%%% @param Widget The frame widget map containing position, dimensions, and optional title
%%% @returns ok
%%% @end
-spec render(map()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    Width = maps:get(width, Widget, 0),
    Height = maps:get(height, Widget, 0),

    if Width > 1 andalso Height > 1 ->
        FrameTitle = maps:get(text, Widget, <<"Untitled">>),

        [?TERMBOX:tb_set_cell(X1, Y1, $_, 1, 2) || X1 <- lists:seq(X, X + Width - 1),
                                                   Y1 <- lists:seq(Y, Y + Height - 1)],
        Box = box_styles:double(),

        table:draw_table(X, Y, Height - 1, Fg, Bg, Box, [Width - 2]),
        ?TERMBOX:tb_print(X + 2, Y, Fg, Bg, FrameTitle);
    true ->
        ok
    end.
