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
    Text = maps:get(text, State, ""),
    CursorPos = maps:get(cursor_pos, State, length(Text)),
    {Before, After} = lists:split(CursorPos, Text),
    NewText = Before ++ binary_to_list(Key) ++ After,
    State#{text => NewText, cursor_pos => CursorPos + byte_size(Key)};
handle_event({key, _, _, _, _, backspace_key}, State) ->
    handle_backspace(State);
handle_event({key, _, _, _, _, backspace2_key}, State) ->
    handle_backspace(State);
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

If wrap is enabled and width/height are set by layout:
- Text wraps at word boundaries at the widget's width
- Only the last N lines (fitting in height) are displayed
""".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = get_text(Widget),
    Wrap = maps:get(wrap, Widget, false),

    case Wrap of
        true ->
            % Wrap text at widget width (set by layout) and render each line
            case maps:get(width, Widget, undefined) of
                undefined ->
                    % Width not set by layout, render as single line
                    cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer);
                Width ->
                    Height = maps:get(height, Widget, undefined),
                    TextBin = list_to_binary(Text),
                    Lines = greedy_wrap:word_wrap(TextBin, Width),
                    % Only show the last N lines that fit in the height (if height is set)
                    VisibleLines = case Height of
                        undefined -> Lines;  % Show all lines if no height constraint
                        _ -> get_last_n_lines(Lines, Height)
                    end,
                    render_lines(X, Y, Fg, Bg, VisibleLines, Buffer)
            end;
        false ->
            % No wrapping - render as single line
            cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer)
    end.

-doc """
Renders the text input widget (focused state) with cursor.

If wrap is enabled and width/height are set by layout:
- Text wraps at word boundaries at the widget's width
- Only the last N lines (fitting in height) are displayed
- Cursor position is calculated correctly across wrapped lines
""".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = get_text(Widget),
    CursorPos = get_cursor_pos(Widget, Text),
    Wrap = maps:get(wrap, Widget, false),

    case Wrap of
        true ->
            % Wrap text and find cursor position
            case maps:get(width, Widget, undefined) of
                undefined ->
                    % Width not set by layout, render as single line with cursor
                    Buffer1 = cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer),
                    {Char, NewX} = case CursorPos >= length(Text) of
                                       true -> {$ , X + length(Text)};
                                       false -> {lists:nth(CursorPos + 1, Text), X + CursorPos}
                                   end,
                    cellium_buffer:set_cell(NewX, Y, Char, Bg, Fg, Buffer1);
                Width ->
                    Height = maps:get(height, Widget, undefined),
                    TextBin = list_to_binary(Text),
                    Lines = greedy_wrap:word_wrap(TextBin, Width),

                    % Find which line the cursor is on (in full wrapped content)
                    {CursorLine, CursorCol} = find_cursor_position(Lines, CursorPos),

                    % Only show the last N lines that fit in the height (if height is set)
                    VisibleLines = case Height of
                        undefined -> Lines;
                        _ -> get_last_n_lines(Lines, Height)
                    end,

                    % Calculate the offset to adjust cursor Y position
                    TotalLines = length(Lines),
                    VisibleLineCount = length(VisibleLines),
                    LineOffset = TotalLines - VisibleLineCount,

                    % Render the visible lines
                    Buffer1 = render_lines(X, Y, Fg, Bg, VisibleLines, Buffer),

                    % Only show cursor if it's in the visible lines
                    case CursorLine >= LineOffset of
                        true ->
                            % Cursor is visible
                            AdjustedCursorY = Y + (CursorLine - LineOffset),
                            CursorX = X + CursorCol,

                            % Get character at cursor position or space if at end
                            VisibleLineIndex = CursorLine - LineOffset + 1,
                            case VisibleLineIndex =< length(VisibleLines) of
                                true ->
                                    Line = lists:nth(VisibleLineIndex, VisibleLines),
                                    LineStr = binary_to_list(Line),
                                    Char = case CursorCol >= length(LineStr) of
                                               true -> $ ;
                                               false -> lists:nth(CursorCol + 1, LineStr)
                                           end,
                                    cellium_buffer:set_cell(CursorX, AdjustedCursorY, Char, Bg, Fg, Buffer1);
                                false ->
                                    Buffer1
                            end;
                        false ->
                            % Cursor is not in visible area (scrolled off top)
                            Buffer1
                    end
            end;
        false ->
            % No wrapping - render as single line with cursor
            Buffer1 = cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer),
            {Char, NewX} = case CursorPos >= length(Text) of
                               true -> {$ , X + length(Text)};
                               false -> {lists:nth(CursorPos + 1, Text), X + CursorPos}
                           end,
            cellium_buffer:set_cell(NewX, Y, Char, Bg, Fg, Buffer1)
    end.

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
