%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(widget_demo). 

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start() ->
   cellium:start(#{module => ?MODULE, color_type => truecolor}).

init(_Args) ->
    WidgetList = ["colors", "box", "frame", "text", "simple" ],
    Model = #{widget => "text", widget_list => WidgetList},
    {ok, Model}.

%% this function mutates the model.
update(#{widget := _WidgetName, widget_list := WidgetList} = Model, Msg) ->
    case Msg of
        {tb_event, key, _ ,{keydata, _ ,$r}} ->
            init("arg");
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            logger:info("Changing widget...", []),
            [FirstWidget | RestWidgets ] = WidgetList,
            NewWidgetList = RestWidgets ++ [FirstWidget],
            #{widget => FirstWidget, widget_list => NewWidgetList}
    end.


render(#{widget := WidgetName}) ->

    FileName = WidgetName ++ ".txt",
    RelativePath = filename:join(["./examples/widgets/", FileName]),
    logger:info("Showing widget: ~p" , [FileName]),

    {ok, [W]} = file:consult(RelativePath),

    logger:info("Widget is: ~p~n", [W]),

    #{type => container,
      id => main_container,
      orientation => horizontal,
      debug => true,
      children => [ W ]}.

