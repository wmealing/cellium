%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(text_demo). 

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start() ->
   cellium:start(#{module => ?MODULE}).

init(_Args) ->
    Model = #{},
    {ok, Model}.

%% this function mutates the model.
update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"r">>} ->
            init("arg");
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            logger:info("Changing text...", []),
	    Model 
    end.


render(_Model) ->
    RelativePath = filename:join(["./examples/texts/", "foo.txt"]),

    {ok, TextList} = read_file_to_binary(RelativePath),

    Text = iolist_to_binary(TextList),

    logger:info(Text),

    TextWidget = #{type => widget,
                   widget_type => text, 
                   id => text_widget,
                   expand => true, 
                   debug => true,
                   value => Text},

    #{type => container,
      id => main_container,
      orientation => horizontal,
      expand => true, 
      debug => false,
      children => [ TextWidget ]}.


read_file_to_binary(Filename) ->
    case file:read_file(Filename) of
        {ok, BinaryData} ->
            {ok, BinaryData};
        {error, Reason} ->
            {error, Reason}
    end.
