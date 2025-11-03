%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% @doc
%%% Floating container demo - displays a floating box at position (5, 5)
%%% @end
%%% Created : 17 Oct 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(floating).

%% API
-export([init/1, render/1, update/2, start/0]).

-behavior(cellium).
-include("cellium.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(_Args) ->
    Model = #{},
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {tb_event, key, _ ,{keydata, _ ,$q}} ->
            cellium:stop(),
            Model;
        _AnythingElse ->
            Model
    end.


render(_Model) ->
    Boxes = boxes(),
    FloatingBox = floating_container:new(float_box, horizontal, 5, 5, 30, 8),
    FloatingBoxWithChildren =
        FloatingBox#{
        children => [
            #{type => widget,
              widget_type => frame,
              position => absolute,
              text => <<"float">>,
              id => content,
              value => <<"This is a floating box!">>,
              x => 10,
              y => 20,
              width => 10,
              height => 5}
        ]
    },

    #{type => container,
      id => main_container,
      orientation => horizontal,
      children => [
                   Boxes,
                   FloatingBoxWithChildren
                  ]}.



start() ->
    cellium:start(#{module => ?MODULE}).


%% internal
boxes() ->
    (container:new(container1, vertical))#{id => outer,
                                           expand => true,
                                           children => [

    (container:new(container2, horizontal))#{id => inner1,
                                             expand => true,
                                             children => [
                                                          model:maybe_set_focus((box:new(box1, 10, 10))#{ expand => true })
                                                          ]},

     (container:new(container2, horizontal))#{id => inner2,
                                             expand => true,
                                             children => [
                                                          model:maybe_set_focus((box:new(box2, 10, 10))#{ expand => true }),
                                                          model:maybe_set_focus((box:new(box3, 10, 10))#{ expand => true }),
                                                          model:maybe_set_focus((box:new(box4, 10, 10))#{ expand => true })
                                                          ]},
     (container:new(container2, horizontal))#{id => inner3,
                                             expand => true,
                                             children => [
                                                          model:maybe_set_focus((box:new(box5, 10, 10))#{ size => 20 }),
                                                          model:maybe_set_focus((box:new(box6, 10, 10))#{ expand => true })
                                                          ]}
                                                       ]}.
