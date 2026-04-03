-module(widgets_gallery).

-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{
        checked => true,
        selected_option => a,
        progress => 30,
        features => true,
        advanced => false,
        spinner_frame => 0,
        toggle_on => false,
        input_text => text_input:state("Edit me!"),
        gauge_value => 50,
        focused_id => undefined,
        active_tab => 0
    },
    % Start a timer for the spinner
    erlang:send_after(100, self(), tick),
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        % Shift + Right Arrow to next tab
        {key, true, false, false, false, right_key} ->
            Active = maps:get(active_tab, Model, 0),
            Model#{active_tab => (Active + 1) rem 3};
        % Shift + Left Arrow to previous tab
        {key, true, false, false, false, left_key} ->
            Active = maps:get(active_tab, Model, 0),
            Model#{active_tab => (Active + 2) rem 3};
        tick ->
            erlang:send_after(100, self(), tick),
            Model#{spinner_frame => maps:get(spinner_frame, Model) + 1};
        {focus_changed, NewFocusedId} ->
            Model#{focused_id => NewFocusedId};
        {key, _, _, _, _, _} = KeyEvent ->
            case focus_manager:get_focused() of
                {ok, Id} -> handle_focused_key(Id, KeyEvent, Model);
                _ -> Model
            end;
        _ ->
            Model
    end.

handle_focused_key(ti1, Event, Model) ->
    NewTextState = text_input:handle_event(Event, maps:get(input_text, Model)),
    Model#{input_text => NewTextState};
handle_focused_key(g1, {key, _, _, _, _, left_key}, Model) ->
    NewValue = max(0, maps:get(gauge_value, Model) - 5),
    Model#{gauge_value => NewValue};
handle_focused_key(g1, {key, _, _, _, _, right_key}, Model) ->
    NewValue = min(100, maps:get(gauge_value, Model) + 5),
    Model#{gauge_value => NewValue};
handle_focused_key(pb1, {key, _, _, _, _, left_key}, Model) ->
    NewValue = max(0, maps:get(progress, Model) - 5),
    Model#{progress => NewValue};
handle_focused_key(pb1, {key, _, _, _, _, right_key}, Model) ->
    NewValue = min(100, maps:get(progress, Model) + 5),
    Model#{progress => NewValue};
handle_focused_key(Id, {key, _, _, _, _, Key}, Model) when Key == enter_key; Key == <<" ">> ->
    case Id of
        r1 -> Model#{selected_option => a};
        r2 -> Model#{selected_option => b};
        cb1 -> Model#{features => not maps:get(features, Model)};
        cb2 -> Model#{advanced => not maps:get(advanced, Model)};
        tg1 -> Model#{toggle_on => not maps:get(toggle_on, Model)};
        _ -> Model
    end;
handle_focused_key(_Id, _Event, Model) ->
    Model.

render(Model) ->
    FocusedId = maps:get(focused_id, Model, undefined),
    ActiveTab = maps:get(active_tab, Model, 0),
    StatusText = io_lib:format("Shift+Arrows: Tabs | Q: Quit | Focused: ~p", [FocusedId]),

    {vbox, [{id, main}, {padding, 0}], [
        {tabs, [{id, gallery_tabs}, {tabs, ["Basic", "Progress", "Input"]}, {active_tab, ActiveTab}, {expand, true}], [
            % Tab 1: Basic Widgets
            {vbox, [{id, tab_basic}, {expand, true}, {padding, 1}], [
                {header, [{id, h1}, {color, cyan}], "Basic Controls"},
                {spacer, [{size, 1}]},

                {hbox, [{id, row1}, {size, 4}], [
                    {vbox, [{id, col1}, {expand, true}], [
                        {text,  [{id, t1}], "Radio Options:"},
                        {radio, [{id, r1}, {selected, maps:get(selected_option, Model) == a}], "Option A"},
                        {radio, [{id, r2}, {selected, maps:get(selected_option, Model) == b}], "Option B"}
                    ]},
                    {vbox, [{id, col2}, {expand, true}], [
                        {text,  [{id, t_cb}], "Features:"},
                        {checkbox, [{id, cb1}, {checked, maps:get(features, Model)}], "Enable Feature"},
                        {checkbox, [{id, cb2}, {checked, not maps:get(advanced, Model)}], "Show Advanced"}
                    ]}
                ]},

                {spacer, [{size, 1}]},

                {hbox, [{id, row2}, {size, 1}], [
                    {button, [{id, btn1}, {color, green}, {size, 12}], "Submit"},
                    {spacer,[{size, 2}]},
                    {button, [{id, btn2}, {color, red}, {size, 12}], "Cancel"}
                ]},
                {spacer, [{expand, true}]}
            ]},

            % Tab 2: Progress & Status
            {vbox, [{id, tab_progress}, {expand, true}, {padding, 1}], [
                {header, [{id, h2}, {color, green}], "Progress & Status"},
                {spacer, [{size, 1}]},
                {vbox, [{id, row3}, {size, 5}], [
                    {text, [{id, t2}], "Progress & Status indicators:"},
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
                {spacer, [{expand, true}]}
            ]},

            % Tab 3: Input & Frames
            {vbox, [{id, tab_input}, {expand, true}, {padding, 1}], [
                {header, [{id, h3}, {color, yellow}], "Complex Layouts"},
                {spacer, [{size, 1}]},
                {vbox, [{id, row4}, {expand, true}], [
                    {text, [{id, t5}], "Input Field inside a Frame:"},
                    {frame, [{id, f_input}, {title, "Input Frame"}, {size, 8}, {color, yellow}], [
                        {text_input, [{id, ti1}, {state, maps:get(input_text, Model)}, {wrap, true}, {expand, true}]}
                    ]},
                    {spacer, [{size, 1}]}
                ]},
                {spacer, [{expand, true}]}
            ]}
        ]},
        {status_bar, [{id, sb1}, {color, white}], lists:flatten(StatusText)}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{
        module => ?MODULE,
        auto_focus => true
    }).
