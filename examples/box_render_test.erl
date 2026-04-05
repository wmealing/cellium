%% Test example for the new box_styles:render_box function
-module(box_render_test).
-behaviour(cellium).

-export([init/1, update/2, render/1, start/0]).

-include("cellium.hrl").

start() ->
    cellium:start(#{module => ?MODULE}).

init(_Args) ->
    {ok, #{test => "box_render"}}.

update(Model, {key, _, _, _, _, <<"q">>}) ->
    cellium:stop(),
    Model;
update(Model, _Msg) ->
    Model.

render(_Model) ->
    {vbox, [{padding, 2}], [
        {text, [], "Testing box_styles:render_box/10"},
        {spacer, [{size, 1}]},
        {custom_box, [{id, box1}, {size, 5}, {title, ""}, {style, square}]},
        {spacer, [{size, 1}]},
        {custom_box, [{id, box2}, {size, 5}, {title, "Left Title"}, {title_align, left}, {style, double}]},
        {spacer, [{size, 1}]},
        {custom_box, [{id, box3}, {size, 5}, {title, "Center Title"}, {title_align, center}, {style, rounded}]},
        {spacer, [{size, 1}]},
        {custom_box, [{id, box4}, {size, 5}, {title, "Right Title"}, {title_align, right}, {style, heavy}]},
        {spacer, [{size, 1}]},
        {text, [], "Press 'q' to quit"}
    ]}.
