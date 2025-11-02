-module(progress_demo).

-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{progress => 0},
    {ok, Model}.

update(#{progress := Progress} = Model, Msg) ->
    case Msg of
        {tb_event, key, _, {keydata, _, $+}} ->
            Model#{progress => clamp(Progress + 5)};
        {tb_event, key, _, {keydata, _, $-}} ->
            Model#{progress => clamp(Progress - 5)};
        {tb_event, key, _, {keydata, _, $q}} ->
            cellium:stop(),
            Model;
        _Else ->
            Model
    end.

render(#{progress := Progress}) ->
    ProgressText = io_lib:bformat("Progress: ~p%", [Progress]),
    BarDisplay = build_bar(Progress),
    Instructions = <<"Use +/- to adjust, q to quit">>,
    
    #{type => container,
      id => main_container,
      orientation => vertical,
      size => 5,
      children => [
                   #{type => widget,
                     widget_type => text,
                     id => title,
                     size => 1,
                     value => ProgressText},
                   #{type => widget,
                     widget_type => text,
                     size => 1,
                     id => bar,
                     value => BarDisplay},
                   #{type => widget,
                     widget_type => text,
                     size => 4,
                     id => instructions,
                     value => Instructions}
                  ]}.

start() ->
    cellium:start(#{module => ?MODULE}).

clamp(V) when V < 0 ->
    0;
clamp(V) when V > 100 ->
    100;
clamp(V) ->
    V.

build_bar(Percentage) ->
    Filled = calculate_filled(Percentage),
    Empty = 20 - Filled,
    FilledChars = lists:duplicate(Filled, 9608),
    EmptyChars = lists:duplicate(Empty, 9617),
    AllChars = FilledChars ++ EmptyChars,
    unicode:characters_to_binary(AllChars).

calculate_filled(Percentage) ->
    round((Percentage / 100) * 20).
