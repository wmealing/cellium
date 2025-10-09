%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(time).

% API
-export([render/1]).

-include("cellium.hrl").

render(Widget) ->
    X = maps:get(x, Widget, 0),
    Y = maps:get(y, Widget, 0),

    #{bg := Bg, fg := Fg} = theme:load(time),

    {{_Year,_Mon,_Day} ,{Hour, Min, Second}} =
        calendar:local_time(),
    Time = io_lib:format("~3..0w:~2..0w:~2..0w", [Hour,Min,Second]),
    TimeBin = binary:list_to_bin(Time),

    ?TERMBOX:tb_print(X + 1,
                      Y + 1,
                      Fg,
                      Bg,
                      TimeBin).
