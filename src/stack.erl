-module(stack).
-export([new/1, render/2]).

new(Id) ->
    (container:new(Id, horizontal))#{
        widget_type => ?MODULE,
        type => container
    }.

render(_Widget, Buffer) ->
    Buffer.
