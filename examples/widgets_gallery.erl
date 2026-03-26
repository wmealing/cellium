-module(widgets_gallery).

-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{
        checked => true,
        selected_option => a,
        progress => 30,
        spinner_frame => 0,
        toggle_on => false,
        input_text => "Edit me!",
        gauge_value => 50
    },
    % Start a timer for the spinner
    erlang:send_after(100, self(), tick),
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        tick ->
            erlang:send_after(100, self(), tick),
            Model#{spinner_frame => maps:get(spinner_frame, Model) + 1};
        {focus_changed, _} ->
            Model;
        % Handle interactions (demo only, normally we'd check which widget is focused)
        {key, _, _, _, _, enter_key} ->
            % Toggle some values for demo
            Model#{
                checked => not maps:get(checked, Model),
                toggle_on => not maps:get(toggle_on, Model),
                progress => (maps:get(progress, Model) + 10) rem 101
            };
        {key, _, _, _, _, left_key} ->
            NewValue = max(0, maps:get(gauge_value, Model) - 5),
            Model#{gauge_value => NewValue};
        {key, _, _, _, _, right_key} ->
            NewValue = min(100, maps:get(gauge_value, Model) + 5),
            Model#{gauge_value => NewValue};
        _ ->
            Model
    end.

render(Model) ->
    {vbox, [{id, main}, {padding, 1}], [
        {header, [{id, h1}, {color, cyan}], "Cellium Widget Gallery"},
        {spacer, [{size, 1}]},
        
        {hbox, [{id, row1}, {size, 3}], [
            {vbox, [{id, col1}, {expand, true}], [
                {button, [{id, btn1}, {color, green}], "Submit"},
                {button, [{id, btn2}, {color, red}], "Cancel"}
            ]},
            {vbox, [{id, col2}, {expand, true}], [
                {checkbox, [{id, cb1}, {checked, maps:get(checked, Model)}], "Enable Feature"},
                {checkbox, [{id, cb2}, {checked, not maps:get(checked, Model)}], "Show Advanced"}
            ]}
        ]},

        {spacer, [{size, 1}]},

        {hbox, [{id, row2}, {size, 1}], [
            {text, [{id, t1}], "Radio Options: "},
            {radio, [{id, r1}, {selected, maps:get(selected_option, Model) == a}], "Option A"},
            {spacer, [{size, 2}]},
            {radio, [{id, r2}, {selected, maps:get(selected_option, Model) == b}], "Option B"}
        ]},

        {spacer, [{size, 1}]},

        {vbox, [{id, row3}, {size, 4}], [
            {text, [{id, t2}], "Progress & Status:"},
            {progress_bar, [{id, pb1}, {progress, maps:get(progress, Model) / 100.0}, {width, 40}]},
            {gauge, [{id, g1}, {value, maps:get(gauge_value, Model)}, {label, <<"Volume">>} , {width, 40}]},
            {hbox, [{id, row3b}], [
                {text, [{id, t3}, {size, 9}], "Loading: "},
                {spinner, [{id, s1}, {frame, maps:get(spinner_frame, Model)}]},
                {spacer, [{size, 5}]},
                {text, [{id, t4}, {size, 8}], "Switch: "},
                {toggle, [{id, tg1}, {on, maps:get(toggle_on, Model)}]}
            ]}
        ]},

        {spacer, [{size, 1}]},

        {vbox, [{id, row4}, {expand, true}], [
            {text, [{id, t5}], "Input Field:"},
            {text_input, [{id, ti1}, {text, maps:get(input_text, Model)}, {color, yellow}]},
            {spacer, [{size, 1}]},
            {box, [{id, b1}, {expand, true}, {color, blue}]}
        ]}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{
        module => ?MODULE,
        auto_focus => true
    }).
