-module(table_cell).
-moduledoc """
A simple widget that renders a piece of text within a bounded cell.

This widget is the most basic building block for tables. It takes a text
string and renders it at a given position, truncating it if it exceeds
the specified width.
""".

-export([render/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1, color_to_int/1]).

-type widget() :: #{
    type := table_cell,
    x := integer(),
    y := integer(),
    width := integer(),
    text := string(),
    selected => boolean(),
    color => atom(),
    'background-color' => atom(),
    cursor_pos => integer()
}.

-doc """
Renders the text content of a single cell.

The `Widget` map is expected to have the following keys:
- `x`: The starting X coordinate.
- `y`: The starting Y coordinate.
- `width`: The maximum width of the cell.
- `text`: The string to be rendered.
- `color` (optional): The foreground color. Defaults to `white`.
- `background-color` (optional): The background color. Defaults to `black`.
- `cursor_pos` (optional): The 0-based index of the cursor within the text.
""".
-spec render(widget()) -> ok.
render(Widget) ->
    #{x := X, y := Y, fg := FgAtom, bg := Bg} = get_common_props(Widget),
    Width = maps:get(width, Widget),
    Text = maps:get(text, Widget, <<"">>),
    IsSelected = maps:get(selected, Widget, false),
    CursorPos = maps:get(cursor_pos, Widget, -1),

    Fg = case IsSelected of
        true -> widget:color_to_int(FgAtom) bor ?TB_REVERSE;
        false -> FgAtom
    end,

    TruncatedText = case byte_size(Text) > Width of
        true -> binary:part(Text, 0, Width);
        false -> Text
    end,

    ?TERMBOX:tb_print(X, Y, Fg, Bg, TruncatedText),

    % Draw spaces to clear the rest of the cell
    ClearWidth = Width - byte_size(TruncatedText),
    if ClearWidth > 0 ->
        ?TERMBOX:tb_print(X + byte_size(TruncatedText), Y, Fg, Bg, lists:duplicate(ClearWidth, " "));
    true ->
        ok
    end,

    if CursorPos =/= -1 andalso CursorPos >= 0 andalso CursorPos =< byte_size(TruncatedText) ->
        ?TERMBOX:tb_set_cursor(X + CursorPos, Y);
    true ->
        ok
    end,
    ok.
