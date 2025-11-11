%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(time).

% API
-export([render/1]).

-import(widget, [get_common_props/1]).

-include("cellium.hrl").

render(Widget) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget), 

    {{_Year,_Mon,_Day} ,{Hour, Min, Second}} =
        calendar:local_time(),
    Time = io_lib:format("~3..0w:~2..0w:~2..0w", [Hour,Min,Second]),
    TimeBin = binary:list_to_bin(Time),

    ?TERMBOX:tb_print(X + 1,
                      Y + 1,
                      Fg,
                      Bg,
                      TimeBin).
