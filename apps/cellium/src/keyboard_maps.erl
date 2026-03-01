-module(keyboard_maps).
-moduledoc """
A module to map macOS specific character inputs from the Option (Alt) key
to their corresponding keyboard events.
""".
-export([parse_alt_char/1]).

alt_map() ->
    #{
        %% option + letter
        229 => {key, [alt_key], <<"a">>},
        8747 => {key, [alt_key], <<"b">>},
        231 => {key, [alt_key], <<"c">>},
        8706 => {key, [alt_key], <<"d">>},
        402 => {key, [alt_key], <<"f">>},
        169 => {key, [alt_key], <<"g">>},
        729 => {key, [alt_key], <<"h">>},
        8710 => {key, [alt_key], <<"j">>},
        730 => {key, [alt_key], <<"k">>},
        172 => {key, [alt_key], <<"l">>},
        181 => {key, [alt_key], <<"m">>},
        248 => {key, [alt_key], <<"o">>},
        960 => {key, [alt_key], <<"p">>},
        339 => {key, [alt_key], <<"q">>},
        174 => {key, [alt_key], <<"r">>},
        223 => {key, [alt_key], <<"s">>},
        8224 => {key, [alt_key], <<"t">>},
        8730 => {key, [alt_key], <<"v">>},
        8721 => {key, [alt_key], <<"w">>},
        8776 => {key, [alt_key], <<"x">>},
        165 => {key, [alt_key], <<"y">>},
        937 => {key, [alt_key], <<"z">>},

        %% option + number
        161 => {key, [alt_key], <<"1">>},
        8482 => {key, [alt_key], <<"2">>},
        163 => {key, [alt_key], <<"3">>},
        162 => {key, [alt_key], <<"4">>},
        8734 => {key, [alt_key], <<"5">>},
        167 => {key, [alt_key], <<"6">>},
        182 => {key, [alt_key], <<"7">>},
        8226 => {key, [alt_key], <<"8">>},
        170 => {key, [alt_key], <<"9">>},
        186 => {key, [alt_key], <<"0">>},

        %% Dead keys (option + e, u, i, n, `) are handled by terminal,
        %% resulting chars are not mapped here to avoid ambiguity.
        %% For example, `option+e` then `a` results in `á`.
        %% The application will receive `á`, not the key sequence.
        %% We are not mapping á to alt+a because it could be typed directly.
        %% The user can enable alt-input-mode to get these mappings.

        %% option + symbol
        8211 => {key, [alt_key], <<"-">>},
        8800 => {key, [alt_key], <<"=">>},
        8230 => {key, [alt_key], <<";">>},
        8217 => {key, [alt_key], <<"'">>},
        8220 => {key, [alt_key], <<"[" >>},
        8216 => {key, [alt_key], <<"]">>},
        171 => {key, [alt_key], <<"\\">>},
        8804 => {key, [alt_key], <<",">>},
        8805 => {key, [alt_key], <<".">>},
        247 => {key, [alt_key], <<"\/">>},

        %% option + shift + letter
        197 => {key, [alt_key, shift_key], <<"A">>},
        199 => {key, [alt_key, shift_key], <<"C">>},
        216 => {key, [alt_key, shift_key], <<"O">>},
        338 => {key, [alt_key, shift_key], <<"Q">>},
        8240 => {key, [alt_key, shift_key], <<"R">>},
        9674 => {key, [alt_key, shift_key], <<"V">>},

        %% option + shift + number
        8364 => {key, [alt_key, shift_key], <<"2">>},
        8249 => {key, [alt_key, shift_key], <<"3">>},
        8250 => {key, [alt_key, shift_key], <<"4">>},
        64257 => {key, [alt_key, shift_key], <<"5">>},
        64258 => {key, [alt_key, shift_key], <<"6">>},
        176 => {key, [alt_key, shift_key], <<"8">>},
        183 => {key, [alt_key, shift_key], <<"9">>},
        8218 => {key, [alt_key, shift_key], <<"0">>},

        %% option + shift + symbol
        8212 => {key, [alt_key, shift_key], <<"-">>},
        177 => {key, [alt_key, shift_key], <<"=">>},
        123 => {key, [alt_key, shift_key], <<"[" >>},
        125 => {key, [alt_key, shift_key], <<"]">>},
        187 => {key, [alt_key, shift_key], <<"\\">>},
        60 => {key, [alt_key, shift_key], <<",">>},
        62 => {key, [alt_key, shift_key], <<".">>},
        191 => {key, [alt_key, shift_key], <<"/">> }
    }.

-spec parse_alt_char(integer()) -> {key, list(), binary() | atom()} | nomatch.
parse_alt_char(Char) ->
    case alt_map() of
        #{Char := Result} -> Result;
        _ -> nomatch
    end.
