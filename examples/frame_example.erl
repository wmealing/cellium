-module(frame_example).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE, 
                    color_type => truecolor,
                    auto_focus => true}).

init(_Ignored) ->
    {ok, #{}}.

update(Model, Msg) ->
    case Msg of
        {key,_ ,_ ,_ ,_,<<"q">>} ->
            cellium:stop(),
            Model;
        _Else ->
            Model
    end.

render(_Model) ->
    {vbox, [{expand, true}], [
        {frame, [{id, left_frame}, {title, "Left Aligned"}, {size, 5}, {color, cyan}], [
            {text, [], "This frame has a left aligned title."}
        ]},
        {frame, [{id, center_frame}, {title, "Center Aligned"}, {size, 5}, {color, green}, {title_align, center}], [
            {text, [], "This frame has a center aligned title."}
        ]},
        {frame, [{id, right_frame}, {title, "Right Aligned"}, {size, 5}, {color, yellow}, {title_align, right}], [
            {text, [], "This frame has a right aligned title."}
        ]},
        {frame, [{id, nested_frame}, {title, "Nested Widgets"}, {expand, true}, {color, magenta}, {title_align, center}], [
            {vbox, [{expand, true}], [
                {text, [], "Inside the frame we have more widgets:"},
                {button, [{id, btn1}], "Button 1"},
                {checkbox, [{id, cb1}], "Checkbox 1"}
            ]}
        ]}
    ]}.
