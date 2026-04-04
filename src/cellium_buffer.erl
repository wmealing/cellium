-module(cellium_buffer).
-moduledoc """
  Shared logic for managing a virtual terminal buffer (shadow buffer).
  Operates on a Map where keys are {X, Y} and values are {Char, Fg, Bg}.
  Supports nested clipping to restrict drawing to specific rectangular areas.
""".

-export([empty/0, set_cell/6, put_string/6, get_cell/3, get_row/3, get_row/4]).
-export([set_clip/5, clear_clip/1]).

-doc "Returns a new empty buffer.".
empty() -> #{}.

-doc """
Sets a rectangular clipping area on the buffer.
If a clip already exists, it intersects the new area with the existing one.
""".
set_clip(X, Y, W, H, Buffer) ->
    case maps:get(clip, Buffer, undefined) of
        undefined ->
            Buffer#{clip => {X, Y, W, H}};
        {CX, CY, CW, CH} ->
            % Intersect new clip with existing clip
            NewX = max(X, CX),
            NewY = max(Y, CY),
            NewRight = min(X + W, CX + CW),
            NewBottom = min(Y + H, CY + CH),
            NewW = max(0, NewRight - NewX),
            NewH = max(0, NewBottom - NewY),
            Buffer#{clip => {NewX, NewY, NewW, NewH}}
    end.

-doc "Clears the clipping area on the buffer.".
clear_clip(Buffer) ->
    maps:remove(clip, Buffer).

-doc "Sets a single cell in the buffer, respecting clip if set.".
set_cell(X, Y, Char, Fg, Bg, Buffer) ->
    case maps:get(clip, Buffer, undefined) of
        undefined ->
            do_set_cell(X, Y, Char, Fg, Bg, Buffer);
        {CX, CY, CW, CH} ->
            case X >= CX andalso X < CX + CW andalso Y >= CY andalso Y < CY + CH of
                true -> do_set_cell(X, Y, Char, Fg, Bg, Buffer);
                false -> Buffer
            end
    end.

do_set_cell(X, Y, Char, Fg, Bg, Buffer) ->
    C = to_char(Char),
    maps:put({X, Y}, {C, Fg, Bg}, Buffer).

-doc "Puts a string into the buffer starting at X, Y, respecting clip if set.".
put_string(X, Y, Fg, Bg, Str, Buffer) ->
    Chars = [ to_char(C) || C <- to_char_list(Str) ],
    lists:foldl(fun({C, Offset}, Acc) ->
        set_cell(X + Offset, Y, C, Fg, Bg, Acc)
    end, Buffer, lists:zip(Chars, lists:seq(0, length(Chars) - 1))).

-doc "Gets a cell from the buffer. Returns space if empty.".
get_cell(X, Y, Buffer) ->
    case maps:get({X, Y}, Buffer, undefined) of
        undefined -> {$\s, default, default};
        Found -> Found
    end.

-doc "Gets an entire row from 0 to Width-1.".
get_row(Y, Width, Buffer) ->
    get_row(0, Y, Width, Buffer).

-doc "Gets a segment of a row starting at StartX with specific length.".
get_row(StartX, Y, Length, Buffer) ->
    [ get_cell(X, Y, Buffer) || X <- lists:seq(StartX, StartX + Length - 1) ].

%% Internal helpers

to_char(Char) when is_binary(Char) -> hd(unicode:characters_to_list(Char));
to_char(Char) when is_integer(Char) -> Char;
to_char(Char) -> Char.

to_char_list(Str) when is_binary(Str) -> unicode:characters_to_list(Str);
to_char_list(Str) when is_list(Str) -> Str;
to_char_list(_) -> [].
