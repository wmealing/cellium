-module(wrap_demo).

-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    Model = #{
        input_text => text_input:state("Edit me!"),
        focused_id => undefined
    },
    % Start a timer for the spinner
    erlang:send_after(100, self(), tick),
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
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
handle_focused_key(_Id, _Event, Model) ->
    Model.

render(Model) ->
    FocusedId = maps:get(focused_id, Model, undefined),
    StatusText = io_lib:format("Type to test wrapping | Q: Quit | Focused: ~p", [FocusedId]),

    {vbox, [{id, main}, {padding, 0}], [
            {vbox, [{id, content}, {expand, true}, {padding, 1}], [
                {text, [{id, t5}], "Input Field with Width & Height Wrapping:"},
                {text, [{id, t6}], "Type a long sentence to see horizontal wrapping."},
                {text, [{id, t7}], "Keep typing to see vertical scrolling (shows last lines)."},
                {spacer, [{size, 1}]},
                {box, [{id, b_input}, {size, 5}, {color, yellow}], [
                    {text_input, [{id, ti1}, {state, maps:get(input_text, Model)}, {wrap, true}, {expand, true}]}
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
