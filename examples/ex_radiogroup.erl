-module(ex_radiogroup).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

init(_) ->
    {ok, #{colour => red, size => small, shape => circle}}.

update(Model, {radiogroup_changed, rg_colour, Value}) ->
    Model#{colour => Value};
update(Model, {radiogroup_changed, rg_size, Value}) ->
    Model#{size => Value};
update(Model, {radiogroup_changed, rg_shape, Value}) ->
    Model#{shape => Value};
update(Model, {key, _, _, _, _, <<"q">>}) ->
    cellium:stop(),
    Model;
update(Model, _) ->
    Model.

render(Model) ->
    Colour = maps:get(colour, Model, red),
    Size   = maps:get(size,   Model, small),
    Shape  = maps:get(shape,  Model, circle),
    {vbox, [{padding, 1}], [
        {header, [{color, cyan}], "Radio Group Example"},
        {spacer, [{size, 1}]},

        {text, [], "Pick a colour (vertical):"},
        {radiogroup, [{id, rg_colour}, {selected, Colour}, {size, 3}],
            [red, green, blue]},

        {spacer, [{size, 1}]},

        {text, [], "Pick a size (horizontal):"},
        {radiogroup, [{id, rg_size}, {selected, Size}, {orientation, horizontal}, {size, 1}],
            [small, medium, large]},

        {spacer, [{size, 1}]},

        {text, [], "Pick a shape (horizontal):"},
        {radiogroup, [{id, rg_shape}, {selected, Shape}, {orientation, horizontal}, {size, 1}],
            [circle, square, triangle]},

        {spacer, [{size, 1}]},

        {text, [{color, yellow}], selection_summary(Colour, Size, Shape)},
        {spacer, [{size, 1}]},
        {text, [], "Tab to switch groups, arrows to change selection."},
        {text, [], "Press 'q' to quit."}
    ]}.

selection_summary(Colour, Size, Shape) ->
    lists:flatten(
        io_lib:format("Selected: ~s ~s ~s",
            [atom_to_list(Size), atom_to_list(Colour), atom_to_list(Shape)])
    ).

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE, auto_focus => true}).
