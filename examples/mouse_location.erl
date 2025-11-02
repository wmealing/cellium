%%%-------------------------------------------------------------------
%%% @author Wae Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(mouse_location).

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{x => 0, y => 0, blocks => []},
    {ok, Model}.

%% this function mutates the model.
update(#{x := X, y := Y, blocks := ExistingBlocks} = Model, Msg) ->
    case Msg of
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            cellium:stop(),
            Model;
        {tb_event, mouse, {buttons, _B}, {pos, {MouseX, MouseY}}} ->
            NewBlocks = ExistingBlocks ++ [#{x => MouseX, y => MouseY}],
            #{x => MouseX, y => MouseY, blocks => NewBlocks };
        _AnythingElse ->
	    logger:info("unknown event: ~p", [Msg]),
            Model
    end.


make_cell(X,Y) ->
    logger:info("make_cell called with X=~p, Y=~p", [X, Y]),
    Ch = $█,
    Cell = (cell:new(foo, Ch))#{x => X,
                         y => Y,
                         size => 1,
                         position => absolute},
    logger:info("Created cell: ~p", [Cell]),
    Cell.

cell_widget(Cells) ->
    lists:map( fun(#{x := X, y := Y} ) -> make_cell(X,Y) end, Cells ).

render(#{x := X, y := Y, blocks := BlockList }) ->
    CounterLabel = io_lib:bformat("Location: ~p:~p ", [X,Y]),
    CellWidgets = cell_widget(BlockList),

    #{type => container,
      id => main_container,
      size => 10,
      orientation => horizontal,
      children => [
                   #{type => widget,
                     widget_type => text,
                     size => 3,
                     id => demo1,
                     value => CounterLabel },
                   #{type        => container,
                     id          => cell_container,
                     expand      => true,
                     orientation => vertical,
                     children    => CellWidgets}]}.

start() ->
   cellium:start(#{module => ?MODULE, report_mouse => true }).
