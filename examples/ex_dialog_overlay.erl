-module(ex_dialog_overlay).
-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    {ok, #{show_dialog => false}}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        {key, _, _, _, _, enter_key} ->
            Model#{show_dialog => not maps:get(show_dialog, Model)};
        {button_clicked, close_btn} ->
            Model#{show_dialog => false};
        _ ->
            Model
    end.

render(#{show_dialog := Show}) ->
    MainContent = {vbox, [{padding, 2}], [
        {header, [{color, green}], "Dialog Overlay Demo"},
        {spacer, [{size, 1}]},
        {text, [], "The dialog should appear OVER this content."},
        {text, [], "Press ENTER to toggle the dialog."},
        {spacer, [{expand, true}]},
        {button, [{id, some_btn}], "Just a background button"}
    ]},

    case Show of
        true ->
            {vbox, [], [
                MainContent,
                {dialog, [{id, my_dialog}, {title, "Modal Dialog"}, {width, 40}, {height, 10}], [
                    {vbox, [{padding, 1}], [
                        {text, [], "I am a dialog drawn in the overlay pass!"},
                        {spacer, [{size, 1}]},
                        {button, [{id, close_btn}], "Close Dialog"}
                    ]}
                ]}
            ]};
        false ->
            MainContent
    end.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{
        module => ?MODULE,
        auto_focus => true
    }).
