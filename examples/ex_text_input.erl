-module(ex_text_input).
-behavior(cellium).
-include("cellium.hrl").
-export([init/1, render/1, update/2, start/0]).

%% The initial state for our example application.
%% We don't need much here as text_input manages its own state
%% in Model#{widget_states => #{Id => State}}.
init(_) -> 
    {ok, #{}}.

%% The update function handles events and updates the model.
update(Model, {key, _, _, _, _, <<"q">>}) ->
    cellium:stop(),
    Model;
update(Model, _) ->
    Model.

%% The render function defines the layout using a DSL.
render(_) ->
    {vbox, [{padding, 1}], [
        {header, [], "Text Input Example"},
        
        {spacer, [{size, 1}]},
        
        {frame, [{id, f1}, {title, "User Information"}, {height, 12}, {color, cyan}], [
            {text, [], "Name (Single line):"},
            {text_input, [{id, name_input}, {height, 1}]},
            
            {spacer, [{size, 1}]},
            
            {text, [], "Bio (Multi-line with wrapping):"},
            {text_input, [{id, bio_input}, {height, 4}, {wrap, true}, {expand, true}]}
        ]},
        
        {spacer, [{size, 1}]},
        
        {text, [], "Instructions:"},
        {text, [], " - Press 'Tab' to switch focus between inputs."},
        {text, [], " - Type text into the focused field."},
        {text, [], " - Use 'Backspace' to delete and arrow keys to move the cursor."},
        {text, [], " - Press 'q' to quit."}
    ]}.

%% Entry point to start the example.
start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{module => ?MODULE}).
