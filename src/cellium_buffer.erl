-module(cellium_buffer).
-moduledoc """
  Shared logic for managing a virtual terminal buffer (shadow buffer).
  Operates on a Map where keys are {X, Y} and values are {Char, Fg, Bg}.
""".

-export([empty/0, set_cell/6, put_string/6, get_cell/3, get_row/3, get_row/4]).

-doc "Returns a new empty buffer.".
empty() -> #{}.

-doc "Sets a single cell in the buffer.".
set_cell(X, Y, Char, Fg, Bg, Buffer) ->
    C = to_char(Char),
    maps:put({X, Y}, {C, Fg, Bg}, Buffer).

-doc "Puts a string into the buffer starting at X, Y.".
put_string(X, Y, Fg, Bg, Str, Buffer) ->
    Chars = [ to_char(C) || C <- to_char_list(Str) ],
    lists:foldl(fun({C, Offset}, Acc) ->
        maps:put({X + Offset, Y}, {C, Fg, Bg}, Acc)
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
