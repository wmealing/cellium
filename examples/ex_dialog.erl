-module(ex_dialog).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

init(_) -> 
    {ok, #{
        active_dialog => none,
        filename => "secret_plans.txt",
        message => "Press 1, 2, or 3 to show different dialogs.",
        
        % Initial states for components in dialogs
        widget_states => #{
            profile_name => #{text => "John Doe"},
            pref_check => #{checked => true},
            lang_list => #{
                items => ["Erlang", "Elixir", "Gleam", "LFE", "Alpaca"],
                selected_index => 0
            }
        }
    }}.

update(Model, {key, _, _, _, _, <<"1">>}) -> Model#{active_dialog => overwrite};
update(Model, {key, _, _, _, _, <<"2">>}) -> Model#{active_dialog => profile};
update(Model, {key, _, _, _, _, <<"3">>}) -> Model#{active_dialog => list};

update(Model, {button_clicked, yes_btn}) ->
    Model#{active_dialog => none, message => "File replaced!"};
update(Model, {button_clicked, no_btn}) ->
    Model#{active_dialog => none, message => "Action cancelled."};
update(Model, {button_clicked, save_btn}) ->
    Model#{active_dialog => none, message => "Profile saved!"};
update(Model, {button_clicked, close_btn}) ->
    Model#{active_dialog => none, message => "Dialog closed."};

update(Model, {key, _, _, _, _, <<"q">>}) ->
    cellium:stop(),
    Model;
update(Model, _) -> 
    Model.

render(Model) ->
    Active = maps:get(active_dialog, Model),
    Message = maps:get(message, Model),
    
    MainContent = [
        {header, [], "Multi-Dialog Example"},
        {spacer, [{size, 1}]},
        {text, [], "1: Overwrite Confirm"},
        {text, [], "2: Profile Settings"},
        {text, [], "3: Language Selection"},
        {spacer, [{size, 2}]},
        {text, [{color, green}], Message},
        {spacer, [{size, 1}]},
        {text, [], "Press 'q' to quit."}
    ],
    
    Dialog = case Active of
        overwrite -> render_overwrite_dialog(Model);
        profile   -> render_profile_dialog();
        list      -> render_list_dialog();
        none      -> []
    end,
    
    {vbox, [{padding, 1}], MainContent ++ Dialog}.

render_overwrite_dialog(Model) ->
    Filename = maps:get(filename, Model),
    [
        {dialog, [{id, dlg_overwrite}, {title, "Confirm Overwrite"}, {width, 40}, {height, 10}, {color, red}], [
            {vbox, [{padding, 1}, {expand, true}], [
                {text, [{wrap, true}], lists:flatten(io_lib:format("File '~s' already exists.", [Filename]))},
                {text, [{wrap, true}], "Do you want to replace it?"},
                {spacer, [{expand, true}]},
                {hbox, [{size, 1}], [
                    {button, [{id, yes_btn}, {size, 10}, {color, green}], "Yes"},
                    {spacer, [{size, 2}]},
                    {button, [{id, no_btn}, {size, 10}, {color, red}], "No"}
                ]}
            ]}
        ]}
    ].

render_profile_dialog() ->
    [
        {dialog, [{id, dlg_profile}, {title, "User Profile"}, {width, 45}, {height, 12}, {color, cyan}], [
            {vbox, [{padding, 1}, {expand, true}], [
                {text, [], "Enter your name:"},
                {text_input, [{id, profile_name}, {expand, true}, {height, 1}]},
                {spacer, [{size, 1}]},
                {checkbox, [{id, pref_check}], "Enable Notifications"},
                {spacer, [{expand, true}]},
                {button, [{id, save_btn}, {size, 1}], "Save Changes"}
            ]}
        ]}
    ].

render_list_dialog() ->
    [
        {dialog, [{id, dlg_list}, {title, "Select Language"}, {width, 35}, {height, 12}, {color, yellow}], [
            {vbox, [{padding, 1}, {expand, true}], [
                {list, [{id, lang_list}, {expand, true}]},
                {spacer, [{size, 1}]},
                {button, [{id, close_btn}, {height, 3}], "Close"}
            ]}
        ]}
    ].

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE}).
