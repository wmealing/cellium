-module(text_input).
-export([render/2, render_focused/2, new/1, handle_event/2, state/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => text_input,
                    text => "",
                    cursor_pos => 0,
                    focusable => true,
                    type => widget}.

-spec state(string()) -> map().
state(Text) -> #{text => Text, cursor_pos => length(Text)}.

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
                    {VisibleLines, LineOffset} = case Height of
                        undefined ->
                            {Lines, 0};  % Show all lines if no height constraint
                        _ ->
                            TotalLines = length(Lines),
                            VLines = get_last_n_lines(Lines, Height),
                            Offset = TotalLines - length(VLines),
                            {VLines, Offset}
                    end,

                    % Adjust cursor line to be relative to visible lines
                    AdjustedCursorLine = CursorLine - LineOffset,

                    % Render visible lines with cursor on the appropriate line (if visible)
                    render_lines_focused(X, Y, Fg, Bg, VisibleLines, AdjustedCursorLine, CursorCol, 0, Buffer)
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

% Get the last N lines from a list
get_last_n_lines(Lines, N) when N >= length(Lines) ->
    Lines;
get_last_n_lines(Lines, N) ->
    lists:nthtail(length(Lines) - N, Lines).

render_lines(_X, _Y, _Fg, _Bg, [], Buffer) ->
    Buffer;
render_lines(X, Y, Fg, Bg, [Line | Rest], Buffer) ->
    LineStr = binary_to_list(Line),
    NewBuffer = cellium_buffer:put_string(X, Y, Fg, Bg, LineStr, Buffer),
    render_lines(X, Y + 1, Fg, Bg, Rest, NewBuffer).

% Find which line and column the cursor is on after wrapping
find_cursor_position(Lines, CursorPos) ->
    find_cursor_position(Lines, CursorPos, 0, 0).

find_cursor_position([], _CursorPos, LineNum, _CharCount) ->
    % Cursor is past the end - put it at the end of the last line
    {LineNum, 0};
find_cursor_position([Line | Rest], CursorPos, LineNum, CharCount) ->
    LineLen = byte_size(Line),
    % Account for space between words (except for last line)
    LineWithSpace = case Rest of
                        [] -> LineLen;
                        _ -> LineLen + 1  % Add 1 for the space
                    end,

    case CursorPos =< CharCount + LineLen of
        true ->
            % Cursor is on this line
            {LineNum, CursorPos - CharCount};
        false ->
            % Cursor is on a later line
            find_cursor_position(Rest, CursorPos, LineNum + 1, CharCount + LineWithSpace)
    end.

render_lines_focused(_X, _Y, _Fg, _Bg, [], _CursorLine, _CursorCol, _CurrentLine, Buffer) ->
    Buffer;
render_lines_focused(X, Y, Fg, Bg, [Line | Rest], CursorLine, CursorCol, CurrentLine, Buffer) ->
    LineStr = binary_to_list(Line),
    Buffer1 = cellium_buffer:put_string(X, Y, Fg, Bg, LineStr, Buffer),

    % If this is the cursor line, draw the cursor
    Buffer2 = case CurrentLine of
                  CursorLine ->
                      {Char, CursorX} = case CursorCol >= length(LineStr) of
                                            true -> {$ , X + length(LineStr)};
                                            false -> {lists:nth(CursorCol + 1, LineStr), X + CursorCol}
                                        end,
                      cellium_buffer:set_cell(CursorX, Y, Char, Bg, Fg, Buffer1);
                  _ ->
                      Buffer1
              end,

    render_lines_focused(X, Y + 1, Fg, Bg, Rest, CursorLine, CursorCol, CurrentLine + 1, Buffer2).
