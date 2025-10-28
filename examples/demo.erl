-module(demo).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

read_file_content(Filename) ->
    case file:read_file(Filename) of
        {ok, BinaryContent} ->
            BinaryContent;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [Filename, Reason]),
            undefined % Or handle the error as needed
    end.


% Example with nested vertical container
nested_layout(_Model) ->
    Sentence = read_file_content("priv/foo.txt"),
    #{
        type => container,
        widget_type => frame,
        class => frame,
        width => 20,
        height => 15,
        id => frame1,
        size => 10,
        children => [
            (container:new(foocontainer, horizontal))#{
                expand => true,
                padding => #{left => 1,right => 0,bottom => 0,top => 1},
                children => [(text:new(foo1, Sentence))#{expand => true}]
            },
            (box:new(foo2, 10, 10))#{size => 50}
        ]
    }.

start() ->
    cellium:start(#{
        module => ?MODULE,
        auto_focus => true
    }).

init(_Ignored) ->
    focus_manager:register_widget(table_demo1),
    focus_manager:register_widget(table_demo2),
    {ok, ""}.

update(Model, _Event) ->
    focus_manager:set_focused(table_demo2),
    Model.

render(Model) ->
    nested_layout(Model).
