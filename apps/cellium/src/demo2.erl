-module(demo2).

%% -behaviour(cellium).

%% -export([init/1, update/2, render/1, go/0]).

%% init(_Args) ->
%%     cellium_event_manager:start_link(),
%%     {ok, #{val => 0}}.

%% update(#{val := Number}, Msg) ->
%%         case Msg of
%%             {event, _Something} ->
%%                 #{val => Number + 1};
%%             _ ->
%%                 #{val => Number - 1}
%%         end.

%% render(#{val := Number}) ->
%%     Widget = #{type => widget,
%%                widget_type => box,
%%                class => box,
%%                width => 10,
%%                height => 10,
%%                size => 10,
%%                x => 0,
%%                y => 0,
%%                id => some_box},

%%     RootWidget = layout:calculate_layout(Widget),


%%     {ok, }

%% go() ->
%%     {ok, Model} = init("hi"),
%%     NewModel = update(Model, foo),
%%     view:start_link(),
%%     render(NewModel).
