-module(text_input).
-moduledoc """
Text input widget module for editable text fields.

This module provides an interactive text input widget that handles keyboard
events and supports optional word wrapping with vertical scrolling.

## Usage

Basic text input:
```
text_input:new(my_input)
```

Text input with wrapping (requires expand or size to get width/height from layout):
```
(text_input:new(my_input))#{wrap => true, expand => true}
```

## Properties

- `wrap` (boolean): Enable word wrapping. When true, text wraps at the widget's
  width and shows only the last N lines that fit in height. Default: false
- `expand` (boolean): Request the layout system to assign width and height.
  Required when using wrap. Default: false
- `width` (integer): Set by the layout system when expand is true. Determines
  wrap width.
- `height` (integer): Set by the layout system when expand is true. Determines
  how many wrapped lines to show (shows last N lines).
- `state` (map): Text input state containing text and cursor position

## Wrapping Behavior

When wrap is enabled:
- Text wraps horizontally at word boundaries (using greedy wrap algorithm)
- Only the last N lines (fitting in height) are displayed
- Earlier lines are hidden but not discarded
- Cursor follows correctly across wrapped lines
""".

-export([render/2, render_focused/2, new/1, handle_event/2, state/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc """
Creates a new text input widget.

To enable word wrapping, set the `wrap` property to true and add `expand`
so the layout system assigns width and height:
```
(text_input:new(my_id))#{wrap => true, expand => true}
```
""".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => text_input,
                    text => "",
                    cursor_pos => 0,
                    focusable => true,
                    type => widget}.

-doc "Creates a text input state with the given text.".
-spec state(string()) -> map().
state(Text) -> #{text => Text, cursor_pos => length(Text)}.

-doc """
Handles keyboard events for the text input.

Processes keyboard input including character insertion, backspace,
and cursor movement (left/right arrow keys).
""".
-spec handle_event(term(), string() | map()) -> string() | map().
handle_event(Event, Text) when is_list(Text) ->
    State = state(Text),
    NewState = handle_event(Event, State),
    maps:get(text, NewState);
handle_event({key, _, _, _, _, Key}, State) when is_binary(Key) ->
    Height = maps:get(height, State, undefined),
    Wrap = maps:get(wrap, State, false),
    % Check if multi-line is allowed
    IsMulti = (Height =:= undefined) orelse (Height > 1) orelse Wrap,
    KeyStr = binary_to_list(Key),
    % Filter out newlines if not multi-line
    FilteredKey = case IsMulti of
        true -> KeyStr;
        false -> [C || C <- KeyStr, C =/= $\n]
    end,
    case FilteredKey of
        [] -> State;
        _ ->
            Text = maps:get(text, State, ""),
            CursorPos = maps:get(cursor_pos, State, length(Text)),
            {Before, After} = lists:split(CursorPos, Text),
            NewText = Before ++ FilteredKey ++ After,
            State#{text => NewText, cursor_pos => CursorPos + length(FilteredKey)}
    end;
handle_event({key, _, _, _, _, backspace_key}, State) ->
    handle_backspace(State);
handle_event({key, _, _, _, _, backspace2_key}, State) ->
    handle_backspace(State);
handle_event({key, _, _, _, _, enter_key}, State) ->
    Height = maps:get(height, State, undefined),
    Wrap = maps:get(wrap, State, false),
    % Only allow newlines if explicitly multi-line (height > 1 or wrap enabled)
    % or if height is undefined (backward compatibility/unbounded height)
    case (Height =:= undefined) orelse (Height > 1) orelse Wrap of
        true ->
            Text = maps:get(text, State, ""),
            CursorPos = maps:get(cursor_pos, State, length(Text)),
            {Before, After} = lists:split(CursorPos, Text),
            NewText = Before ++ "\n" ++ After,
            State#{text => NewText, cursor_pos => CursorPos + 1};
        false ->
            State
    end;
handle_event({key, _, _, _, _, left_key}, State) ->
    CursorPos = maps:get(cursor_pos, State, 0),
    State#{cursor_pos => max(0, CursorPos - 1)};
handle_event({key, _, _, _, _, right_key}, State) ->
    Text = maps:get(text, State, ""),
    CursorPos = maps:get(cursor_pos, State, 0),
    State#{cursor_pos => min(length(Text), CursorPos + 1)};
handle_event(_, State) ->
    State.

handle_backspace(State) ->
    Text = maps:get(text, State, ""),
    CursorPos = maps:get(cursor_pos, State, 0),
    case CursorPos of
        0 -> State;
        _ ->
            {Before, After} = lists:split(CursorPos, Text),
            NewText = lists:sublist(Before, length(Before) - 1) ++ After,
            State#{text => NewText, cursor_pos => CursorPos - 1}
    end.

-doc """
Renders the text input widget (unfocused state).
""".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = get_text(Widget),
    Wrap = maps:get(wrap, Widget, false),
    % Use width/height assigned by layout if available, otherwise default to full terminal or undefined
    Width = maps:get(width, Widget, ?TERMINAL:term_width()),
    Height = maps:get(height, Widget, undefined),

    % 1. Split by explicit newlines
    RawLines = string:split(Text, "\n", all),
    
    % 2. Wrap each part if enabled
    Lines = case Wrap of
        true ->
            lists:foldr(fun(Line, Acc) ->
                Wrapped = greedy_wrap:word_wrap(list_to_binary(Line), Width),
                Wrapped ++ Acc
            end, [], RawLines);
        false ->
            [list_to_binary(L) || L <- RawLines]
    end,

    % 3. Filter for visible lines
    VisibleLines = case Height of
        undefined -> Lines;
        _ -> get_last_n_lines(Lines, Height)
    end,

    render_lines(X, Y, Fg, Bg, VisibleLines, Buffer).

-doc """
Renders the text input widget (focused state) with cursor.
""".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = get_text(Widget),
    CursorPos = get_cursor_pos(Widget, Text),
    Wrap = maps:get(wrap, Widget, false),
    Width = maps:get(width, Widget, ?TERMINAL:term_width()),
    Height = maps:get(height, Widget, undefined),

    % 1. Split and wrap while tracking cursor
    {AllWrappedLines, FinalCursorLine, FinalCursorCol} = 
        wrap_with_cursor(Text, Width, CursorPos, Wrap),

    % 2. Filter for visible lines
    VisibleLines = case Height of
        undefined -> AllWrappedLines;
        _ -> get_last_n_lines(AllWrappedLines, Height)
    end,

    TotalLines = length(AllWrappedLines),
    VisibleLineCount = length(VisibleLines),
    LineOffset = TotalLines - VisibleLineCount,

    % 3. Render the visible lines
    Buffer1 = render_lines(X, Y, Fg, Bg, VisibleLines, Buffer),

    % 4. Only show cursor if it's in the visible lines
    case FinalCursorLine >= LineOffset of
        true ->
            AdjustedCursorY = Y + (FinalCursorLine - LineOffset),
            CursorX = X + FinalCursorCol,

            VisibleLineIndex = FinalCursorLine - LineOffset + 1,
            case VisibleLineIndex =< length(VisibleLines) of
                true ->
                    Line = lists:nth(VisibleLineIndex, VisibleLines),
                    LineStr = binary_to_list(Line),
                    Char = case FinalCursorCol >= length(LineStr) of
                               true -> $ ;
                               false -> lists:nth(FinalCursorCol + 1, LineStr)
                           end,
                    cellium_buffer:set_cell(CursorX, AdjustedCursorY, Char, Bg, Fg, Buffer1);
                false ->
                    Buffer1
            end;
        false ->
            Buffer1
    end.

wrap_with_cursor(Text, Width, CursorPos, Wrap) ->
    RawLines = string:split(Text, "\n", all),
    {AllLines, CLine, CCol, _} = lists:foldl(fun(RawLine, {AccLines, FoundLine, FoundCol, TotalProcessed}) ->
        Wrapped = case Wrap of
            true -> greedy_wrap:word_wrap(list_to_binary(RawLine), Width);
            false -> [list_to_binary(RawLine)]
        end,

        {NewLine, NewCol, NewTotal} = case FoundLine of
            undefined ->
                RawLineLen = length(RawLine),
                case CursorPos =< TotalProcessed + RawLineLen of
                    true ->
                        % Cursor is here! Find which wrapped line it's in.
                        OffsetInRaw = CursorPos - TotalProcessed,
                        {WL, WC} = find_cursor_position(Wrapped, OffsetInRaw),
                        {length(AccLines) + WL, WC, TotalProcessed + RawLineLen + 1};
                    false ->
                        {undefined, undefined, TotalProcessed + RawLineLen + 1}
                end;
            _ ->
                {FoundLine, FoundCol, TotalProcessed + length(RawLine) + 1}
        end,
        {AccLines ++ Wrapped, NewLine, NewCol, NewTotal}
    end, {[], undefined, undefined, 0}, RawLines),

    {FinalLine, FinalCol} = case CLine of
        undefined -> {length(AllLines) - 1, byte_size(lists:last(AllLines))};
        _ -> {CLine, CCol}
    end,

    {AllLines, FinalLine, FinalCol}.

get_text(Widget) ->
    case maps:get(state, Widget, undefined) of
        undefined -> maps:get(text, Widget, "");
        State -> maps:get(text, State, "")
    end.

get_cursor_pos(Widget, Text) ->
    case maps:get(state, Widget, undefined) of
        undefined -> maps:get(cursor_pos, Widget, length(Text));
        State -> maps:get(cursor_pos, State, length(Text))
    end.

get_last_n_lines(Lines, N) ->
    case length(Lines) > N of
        true -> lists:nthtail(length(Lines) - N, Lines);
        false -> Lines
    end.

render_lines(_X, _Y, _Fg, _Bg, [], Buffer) ->
    Buffer;
render_lines(X, Y, Fg, Bg, [Line | Rest], Buffer) ->
    LineStr = binary_to_list(Line),
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, LineStr, Buffer),
    render_lines(X, Y + 1, Fg, Bg, Rest, NewBuffer).

find_cursor_position(Lines, CursorPos) ->
    find_cursor_position(Lines, CursorPos, 0, 0).

find_cursor_position([], _CursorPos, CurrentLine, _CharCount) ->
    % Cursor is past all lines, place at end of last line
    {CurrentLine, 0};
find_cursor_position([Line | Rest], CursorPos, CurrentLine, CharCount) ->
    LineLength = byte_size(Line),
    case CursorPos =< CharCount + LineLength of
        true ->
            % Cursor is on this line
            ColPos = CursorPos - CharCount,
            {CurrentLine, ColPos};
        false ->
            % Cursor is on a later line
            find_cursor_position(Rest, CursorPos, CurrentLine + 1, CharCount + LineLength)
    end.
