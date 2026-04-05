%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(box_styles).
-moduledoc """
Box styles and rendering functions for various border types.
""".

-include("cellium.hrl").

-export([
    ascii/0,
    ascii2/0,
    ascii_double_head/0,
    square/0,
    square_double_head/0,
    minimal/0,
    minimal_heavy_head/0,
    minimal_double_head/0,
    simple/0,
    simple_head/0,
    simple_heavy/0,
    horizontals/0,
    rounded/0,
    heavy/0,
    heavy_edge/0,
    heavy_head/0,
    double/0,
    double_edge/0,
    markdown/0,
    drawline/6,
    draw_horizontal_line/6,
    draw_vertical_line/6,
    render_box/10
]).




ascii() ->
    #box{
        top_left = "+", top = "-", top_divider = "-", top_right = "+",
        head_left = "|", head_vertical = " ", head_right = "|",
        head_row_left = "|", head_row_horizontal = "-", head_row_cross = "+", head_row_right = "|",
        mid_left = "|", mid_vertical = " ", mid_right = "|",
        row_left = "|", row_horizontal = "-", row_cross = "+", row_right = "|",
        foot_row_left = "|", foot_row_horizontal = "-", foot_row_cross = "+", foot_row_right = "|",
        foot_left = "|", foot_vertical = " ", foot_right = "|",
        bottom_left = "+", bottom = "-", bottom_divider = "-", bottom_right = "+",
        is_ascii = true
    }.

ascii2() ->
    #box{
        top_left = "+", top = "-", top_divider = "+", top_right = "+",
        head_left = "|", head_vertical = " ", head_right = "|",
        head_row_left = "+", head_row_horizontal = "-", head_row_cross = "+", head_row_right = "+",
        mid_left = "|", mid_vertical = " ", mid_right = "|",
        row_left = "+", row_horizontal = "-", row_cross = "+", row_right = "+",
        foot_row_left = "+", foot_row_horizontal = "-", foot_row_cross = "+", foot_row_right = "+",
        foot_left = "|", foot_vertical = " ", foot_right = "|",
        bottom_left = "+", bottom = "-", bottom_divider = "+", bottom_right = "+",
        is_ascii = true
    }.

ascii_double_head() ->
    #box{
        top_left = "+", top = "-", top_divider = "+", top_right = "+",
        head_left = "|", head_vertical = " ", head_right = "|",
        head_row_left = "=", head_row_horizontal = "=", head_row_cross = "+", head_row_right = "=",
        mid_left = "|", mid_vertical = " ", mid_right = "|",
        row_left = "+", row_horizontal = "-", row_cross = "+", row_right = "+",
        foot_row_left = "+", foot_row_horizontal = "-", foot_row_cross = "+", foot_row_right = "+",
        foot_left = "|", foot_vertical = " ", foot_right = "|",
        bottom_left = "+", bottom = "-", bottom_divider = "+", bottom_right = "+",
        is_ascii = true
    }.

square() ->
    #box{
        top_left = "┌", top = "─", top_divider = "┬", top_right = "┐",
        head_left = "│", head_vertical = "│", head_right = "│",
        head_row_left = "├", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "┤",
        mid_left = "│", mid_vertical = "│", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = "│", foot_right = "│",
        bottom_left = "└", bottom = "─", bottom_divider = "┴", bottom_right = "┘"
    }.

square_double_head() ->
    #box{
        top_left = "┌", top = "─", top_divider = "┬", top_right = "┐",
        head_left = "│", head_vertical = "│", head_right = "│",
        head_row_left = "╞", head_row_horizontal = "═", head_row_cross = "╪", head_row_right = "╡",
        mid_left = "│", mid_vertical = "│", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = "│", foot_right = "│",
        bottom_left = "└", bottom = "─", bottom_divider = "┴", bottom_right = "┘"
    }.

minimal() ->
    #box{
        top_left = " ", top = " ", top_divider = "╷", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = "│",
        head_row_left = "╶", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "╴",
        mid_left = " ", mid_vertical = " ", mid_right = "│",
        row_left = "╶", row_horizontal = "─", row_cross = "┼", row_right = "╴",
        foot_row_left = "╶", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "╴",
        foot_left = " ", foot_vertical = " ", foot_right = "│",
        bottom_left = " ", bottom = " ", bottom_divider = "╵", bottom_right = " "
    }.

minimal_heavy_head() ->
    #box{
        top_left = " ", top = " ", top_divider = "╷", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = "│",
        head_row_left = "╺", head_row_horizontal = "━", head_row_cross = "┿", head_row_right = "╸",
        mid_left = " ", mid_vertical = " ", mid_right = "│",
        row_left = "╶", row_horizontal = "─", row_cross = "┼", row_right = "╴",
        foot_row_left = "╶", foot_row_horizontal = "─", foot_row_right = "╴",
        foot_left = " ", foot_vertical = " ", foot_right = "│",
        bottom_left = " ", bottom = " ", bottom_divider = "╵", bottom_right = " "
    }.

minimal_double_head() ->
    #box{
        top_left = " ", top = " ", top_divider = "╷", top_right = " ",
        head_left = " ", head_vertical = " ", head_right = "│",
        head_row_left = " ", head_row_horizontal = "═", head_row_cross = "╪", head_row_right = " ",
        mid_left = " ", mid_vertical = " ", mid_right = "│",
        row_left = " ", row_horizontal = "─", row_cross = "┼", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = " ",
        foot_left = " ", foot_vertical = " ", foot_right = "│",
        bottom_left = " ", bottom = " ", bottom_divider = "╵", bottom_right = " "
    }.

simple() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = " ", head_vertical = "│", head_right = " ",
        head_row_left = " ", head_row_horizontal = "─", head_row_cross = "─", head_row_right = " ",
        mid_left = " ", mid_vertical = "│", mid_right = " ",
        row_left = " ", row_horizontal = " ", row_cross = " ", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "─", foot_row_cross = "─", foot_row_right = " ",
        foot_left = " ", foot_vertical = "│", foot_right = " ",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " "
    }.

simple_head() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = " ", head_vertical = "│", head_right = " ",
        head_row_left = " ", head_row_horizontal = "─", head_row_cross = "─", head_row_right = " ",
        mid_left = " ", mid_vertical = " ", mid_right = " ",
        row_left = " ", row_horizontal = " ", row_cross = " ", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = " ", foot_row_cross = " ", foot_row_right = " ",
        foot_left = " ", foot_vertical = " ", foot_right = " ",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " "
    }.

simple_heavy() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = " ", head_vertical = "┃", head_right = " ",
        head_row_left = " ", head_row_horizontal = "━", head_row_cross = "━", head_row_right = " ",
        mid_left = " ", mid_vertical = "┃", mid_right = " ",
        row_left = " ", row_horizontal = " ", row_cross = " ", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "━", foot_row_cross = "━", foot_row_right = " ",
        foot_left = " ", foot_vertical = "┃", foot_right = " ",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " "
    }.

horizontals() ->
    #box{
        top_left = " ", top = "─", top_divider = "─", top_right = " ",
        head_left = " ", head_vertical = "│", head_right = " ",
        head_row_left = " ", head_row_horizontal = "─", head_row_cross = "─", head_row_right = " ",
        mid_left = " ", mid_vertical = "│", mid_right = " ",
        row_left = " ", row_horizontal = "─", row_cross = "─", row_right = " ",
        foot_row_left = " ", foot_row_horizontal = "─", foot_row_cross = "─", foot_row_right = " ",
        foot_left = " ", foot_vertical = "│", foot_right = " ",
        bottom_left = " ", bottom = "─", bottom_divider = "─", bottom_right = " "
    }.


rounded() ->
    #box{
        top_left = "╭", top = "─", top_divider = "┬", top_right = "╮",
        head_left = "│", head_vertical = "│", head_right = "│",
        head_row_left = "├", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "┤",
        mid_left = "│", mid_vertical = "│", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = "│", foot_right = "│",
        bottom_left = "╰", bottom = "─", bottom_divider = "┴", bottom_right = "╯"
    }.

heavy() ->
    #box{
        top_left = "┏", top = "━", top_divider = "┳", top_right = "┓",
        head_left = "┃", head_vertical = "┃", head_right = "┃",
        head_row_left = "┣", head_row_horizontal = "━", head_row_cross = "╋", head_row_right = "┫",
        mid_left = "┃", mid_vertical = "┃", mid_right = "┃",
        row_left = "┣", row_horizontal = "━", row_cross = "╋", row_right = "┫",
        foot_row_left = "┣", foot_row_horizontal = "━", foot_row_cross = "╋", foot_row_right = "┫",
        foot_left = "┃", foot_vertical = "┃", foot_right = "┃",
        bottom_left = "┗", bottom = "━", bottom_divider = "┻", bottom_right = "┛"
    }.

heavy_edge() ->
    #box{
        top_left = "┏", top = "━", top_divider = "┯", top_right = "┓",
        head_left = "┃", head_vertical = "┃", head_right = "┃",
        head_row_left = "┠", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "┨",
        mid_left = "┃", mid_vertical = "┃", mid_right = "┃",
        row_left = "┠", row_horizontal = "─", row_cross = "┼", row_right = "┨",
        foot_row_left = "┠", foot_row_horizontal = "─", foot_row_right = "┨",
        foot_left = "┃", foot_vertical = "┃", foot_right = "┃",
        bottom_left = "┗", bottom = "━", bottom_divider = "┷", bottom_right = "┛"
    }.

heavy_head() ->
    #box{
        top_left = "┏", top = "━", top_divider = "┳", top_right = "┓",
        head_left = "┃", head_vertical = "┃", head_right = "┃",
        head_row_left = "┡", head_row_horizontal = "━", head_row_cross = "╇", head_row_right = "┩",
        mid_left = "│", mid_vertical = "│", mid_right = "│",
        row_left = "├", row_horizontal = "─", row_cross = "┼", row_right = "┤",
        foot_row_left = "├", foot_row_horizontal = "─", foot_row_cross = "┼", foot_row_right = "┤",
        foot_left = "│", foot_vertical = "│", foot_right = "│",
        bottom_left = "└", bottom = "─", bottom_divider = "┴", bottom_right = "┘"
    }.

double() ->
    #box{
        top_left = "╔", top = "═", top_divider = "╦", top_right = "╗",
        head_left = "║", head_vertical = "║", head_right = "║",
        head_row_left = "╠", head_row_horizontal = "═", head_row_cross = "╬", head_row_right = "╣",
        mid_left = "║", mid_vertical = "║", mid_right = "║",
        row_left = "╠", row_horizontal = "═", row_cross = "╬", row_right = "╣",
        foot_row_left = "╠", foot_row_horizontal = "═", foot_row_cross = "╬", foot_row_right = "╣",
        foot_left = "║", foot_vertical = "║", foot_right = "║",
        bottom_left = "╚", bottom = "═", bottom_divider = "╩", bottom_right = "╝"
    }.

double_edge() ->
    #box{
        top_left = "╔", top = "═", top_divider = "╤", top_right = "╗",
        head_left = "║", head_vertical = "║", head_right = "║",
        head_row_left = "╟", head_row_horizontal = "─", head_row_cross = "┼", head_row_right = "╢",
        mid_left = "║", mid_vertical = "║", mid_right = "║",
        row_left = "╟", row_horizontal = "─", row_cross = "┼", row_right = "╢",
        foot_row_left = "╟", foot_row_horizontal = "─", foot_row_right = "╢",
        foot_left = "║", foot_vertical = "║", foot_right = "║",
        bottom_left = "╚", bottom = "═", bottom_divider = "╧", bottom_right = "╝"
    }.

markdown() ->
    #box{
        top_left = " ", top = " ", top_divider = " ", top_right = " ",
        head_left = "|", head_vertical = "|", head_right = "|",
        head_row_left = "|", head_row_horizontal = "-", head_row_cross = "|", head_row_right = "|",
        mid_left = "|", mid_vertical = "|", mid_right = "|",
        row_left = "|", row_horizontal = "-", row_cross = "|", row_right = "|",
        foot_row_left = "|",
        foot_left = "|", foot_vertical = "|", foot_right = "|",
        bottom_left = " ", bottom = " ", bottom_divider = " ", bottom_right = " ",
        is_ascii = true
    }.

drawline(X, Y, Fg, Bg, Line, Buffer) ->
    BinLine = unicode:characters_to_binary(Line),
    cellium_buffer:put_string(X, Y, Fg, Bg, BinLine, Buffer).

draw_horizontal_line(X1, _Y, X2, _Bg, _Fg, Buffer) when X1 > X2 ->
    Buffer;

draw_horizontal_line(X1, Y, X2, Bg, Fg, Buffer) ->
    Buffer1 = cellium_buffer:set_cell(X1, Y, $─, Bg, Fg, Buffer),
    draw_horizontal_line(X1 + 1, Y, X2, Bg, Fg, Buffer1).

draw_vertical_line(Y1, _X, Y2, _Bg, _Fg, Buffer) when Y1 > Y2 ->
    Buffer;

draw_vertical_line(Y1, X, Y2, Bg, Fg, Buffer) ->
    Buffer1 = cellium_buffer:set_cell(X, Y1, $│, Bg, Fg, Buffer),
    draw_vertical_line(Y1 + 1, X, Y2, Bg, Fg, Buffer1).

-doc """
Renders a rectangular box with optional title in the top border.

This is a generic box renderer that handles simple rectangular boxes
with optional title text interrupting the top border.

- `X`: Starting X coordinate (left edge)
- `Y`: Starting Y coordinate (top edge)
- `Width`: Total width of the box (including borders)
- `Height`: Total height of the box (including borders)
- `BoxStyle`: Box style record defining border characters
- `Title`: Optional title text (empty string or binary for no title)
- `TitleAlign`: Title alignment (left | center | right)
- `Fg`: Foreground color
- `Bg`: Background color
- `Buffer`: Current frame buffer
- Returns: Updated buffer
""".
-spec render_box(integer(), integer(), integer(), integer(), #box{},
                 string() | binary(), left | center | right, atom(), atom(), map()) -> map().
render_box(X, Y, Width, Height, BoxStyle, Title, TitleAlign, Fg, Bg, Buffer) ->
    if
        Width < 2 orelse Height < 2 ->
            Buffer;
        true ->
            % Draw top border (with optional title)
            Buffer1 = draw_top_border(X, Y, Width, BoxStyle, Title, TitleAlign, Fg, Bg, Buffer),
            % Draw vertical sides
            Buffer2 = draw_vertical_borders(X, Y + 1, Width, Height - 2, BoxStyle, Fg, Bg, Buffer1),
            % Draw bottom border
            draw_bottom_border(X, Y + Height - 1, Width, BoxStyle, Fg, Bg, Buffer2)
    end.

%%% @private
-doc "Draws the top border line with optional title interruption.".
-spec draw_top_border(integer(), integer(), integer(), #box{},
                      string() | binary(), left | center | right, atom(), atom(), map()) -> map().
draw_top_border(X, Y, Width, BoxStyle, Title, TitleAlign, Fg, Bg, Buffer) ->
    % Top left corner
    Buffer1 = cellium_buffer:set_cell(X, Y,
        unicode:characters_to_list(BoxStyle#box.top_left), Fg, Bg, Buffer),

    % Top right corner
    Buffer2 = cellium_buffer:set_cell(X + Width - 1, Y,
        unicode:characters_to_list(BoxStyle#box.top_right), Fg, Bg, Buffer1),

    % Fill top border (between corners)
    InnerWidth = Width - 2,

    % Handle title interruption
    case normalize_title(Title) of
        "" ->
            % No title, just draw horizontal line
            draw_top_line_segment(X + 1, Y, InnerWidth, BoxStyle#box.top, Fg, Bg, Buffer2);
        TitleStr ->
            % Draw line with title interruption
            draw_top_with_title(X + 1, Y, InnerWidth, BoxStyle#box.top,
                                TitleStr, TitleAlign, Fg, Bg, Buffer2)
    end.

%%% @private
-doc "Draws vertical borders (left and right sides).".
-spec draw_vertical_borders(integer(), integer(), integer(), integer(), #box{},
                            atom(), atom(), map()) -> map().
draw_vertical_borders(_X, _Y, _Width, 0, _BoxStyle, _Fg, _Bg, Buffer) ->
    Buffer;
draw_vertical_borders(X, Y, Width, Height, BoxStyle, Fg, Bg, Buffer) ->
    % Left edge
    Buffer1 = cellium_buffer:set_cell(X, Y,
        unicode:characters_to_list(BoxStyle#box.mid_left), Fg, Bg, Buffer),
    % Right edge
    Buffer2 = cellium_buffer:set_cell(X + Width - 1, Y,
        unicode:characters_to_list(BoxStyle#box.mid_right), Fg, Bg, Buffer1),
    draw_vertical_borders(X, Y + 1, Width, Height - 1, BoxStyle, Fg, Bg, Buffer2).

%%% @private
-doc "Draws the bottom border line.".
-spec draw_bottom_border(integer(), integer(), integer(), #box{}, atom(), atom(), map()) -> map().
draw_bottom_border(X, Y, Width, BoxStyle, Fg, Bg, Buffer) ->
    % Bottom left corner
    Buffer1 = cellium_buffer:set_cell(X, Y,
        unicode:characters_to_list(BoxStyle#box.bottom_left), Fg, Bg, Buffer),

    % Bottom right corner
    Buffer2 = cellium_buffer:set_cell(X + Width - 1, Y,
        unicode:characters_to_list(BoxStyle#box.bottom_right), Fg, Bg, Buffer1),

    % Bottom horizontal line
    draw_top_line_segment(X + 1, Y, Width - 2, BoxStyle#box.bottom, Fg, Bg, Buffer2).

%%% @private
-doc "Draws a horizontal line segment.".
-spec draw_top_line_segment(integer(), integer(), integer(), string(), atom(), atom(), map()) -> map().
draw_top_line_segment(_X, _Y, Width, _Char, _Fg, _Bg, Buffer) when Width =< 0 ->
    Buffer;
draw_top_line_segment(X, Y, Width, Char, Fg, Bg, Buffer) ->
    draw_top_line_segment_loop(X, Y, X + Width - 1, Char, Fg, Bg, Buffer).

%%% @private
-spec draw_top_line_segment_loop(integer(), integer(), integer(), string(), atom(), atom(), map()) -> map().
draw_top_line_segment_loop(X, _Y, EndX, _Char, _Fg, _Bg, Buffer) when X > EndX ->
    Buffer;
draw_top_line_segment_loop(X, Y, EndX, Char, Fg, Bg, Buffer) ->
    Buffer1 = cellium_buffer:set_cell(X, Y, unicode:characters_to_list(Char), Fg, Bg, Buffer),
    draw_top_line_segment_loop(X + 1, Y, EndX, Char, Fg, Bg, Buffer1).

%%% @private
-doc "Draws the top line with title text interrupting it.".
-spec draw_top_with_title(integer(), integer(), integer(), string(),
                          string(), left | center | right, atom(), atom(), map()) -> map().
draw_top_with_title(X, Y, AvailableWidth, LineChar, Title, Align, Fg, Bg, Buffer) ->
    % Add spacing around title
    FormattedTitle = " " ++ Title ++ " ",
    TitleLen = length(FormattedTitle),

    if
        TitleLen >= AvailableWidth ->
            % Title too long, truncate it
            TruncatedTitle = string:slice(FormattedTitle, 0, AvailableWidth),
            cellium_buffer:put_string(X, Y, Fg, Bg, TruncatedTitle, Buffer);
        true ->
            % Calculate title position based on alignment
            {TitleX, LeftPadding, RightPadding} = calculate_title_position(
                X, AvailableWidth, TitleLen, Align),

            % Draw left padding
            Buffer1 = draw_top_line_segment(X, Y, LeftPadding, LineChar, Fg, Bg, Buffer),

            % Draw title
            Buffer2 = cellium_buffer:put_string(TitleX, Y, Fg, Bg, FormattedTitle, Buffer1),

            % Draw right padding
            draw_top_line_segment(TitleX + TitleLen, Y, RightPadding, LineChar, Fg, Bg, Buffer2)
    end.

%%% @private
-doc "Calculates the X position and padding for the title based on alignment.".
-spec calculate_title_position(integer(), integer(), integer(), left | center | right)
    -> {integer(), integer(), integer()}.
calculate_title_position(X, AvailableWidth, TitleLen, left) ->
    % Left align with small offset for aesthetics
    LeftPadding = min(1, AvailableWidth - TitleLen),
    TitleX = X + LeftPadding,
    RightPadding = AvailableWidth - TitleLen - LeftPadding,
    {TitleX, LeftPadding, RightPadding};
calculate_title_position(X, AvailableWidth, TitleLen, center) ->
    % Center the title
    LeftPadding = (AvailableWidth - TitleLen) div 2,
    TitleX = X + LeftPadding,
    RightPadding = AvailableWidth - TitleLen - LeftPadding,
    {TitleX, LeftPadding, RightPadding};
calculate_title_position(X, AvailableWidth, TitleLen, right) ->
    % Right align with small offset
    RightPadding = min(1, AvailableWidth - TitleLen),
    LeftPadding = AvailableWidth - TitleLen - RightPadding,
    TitleX = X + LeftPadding,
    {TitleX, LeftPadding, RightPadding}.

%%% @private
-doc "Normalizes title to a string, handling empty strings and binaries.".
-spec normalize_title(string() | binary()) -> string().
normalize_title("") -> "";
normalize_title(<<>>) -> "";
normalize_title(Title) when is_binary(Title) ->
    unicode:characters_to_list(Title);
normalize_title(Title) when is_list(Title) ->
    Title.
