%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(example_demo). 

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{widget_list => []},
    {ok, Model}.

%% this function mutates the model.
update(#{widget := _WidgetName, widget_list := _WidgetList} = Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            Model
    end.


render(_Model) ->

    W = [ #{id    => text_green,
            type  => widget,
            widget_type => text,
            size  => 1,
            value => <<"GREEN">>,
            color => green,
            'background-color' => black},

          #{id     => text_bright_green,
            type   => widget,
            widget_type => text,
            size   => 1,
            value   => <<"SUPER GREEN">>,
            color  => bright_green,
            'background-color' => black,
            expand => true} ],

    #{type => container,
      id => main_container,
      orientation => vertical,
      size => 10,
      children => W }.


start() ->
   cellium:start(#{module => ?MODULE}).
