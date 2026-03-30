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
    cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer).

-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Text = get_text(Widget),
    CursorPos = get_cursor_pos(Widget, Text),

    % Draw the text first
    Buffer1 = cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer),

    % Draw the cursor as a highlighted character
    % If CursorPos is at the end, draw a space
    {Char, NewX} = case CursorPos >= length(Text) of
                       true -> {$ , X + length(Text)};
                       false -> {lists:nth(CursorPos + 1, Text), X + CursorPos}
                   end,
    cellium_buffer:set_cell(NewX, Y, Char, Bg, Fg, Buffer1).

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
