-module(editor).

%%%-------------------------------------------------------------------
%%% @doc
%%% Text editor example demonstrating user input and state rendering.
%%%
%%% This example shows:
%%% - Accepting character input from the keyboard
%%% - Building a string character by character
%%% - Deleting characters with backspace
%%% - Rendering the text with a cursor indicator
%%%
%%% Usage:
%%%   editor:start()
%%%
%%% Controls:
%%%   Type text    - Input characters and spaces
%%%   Backspace    - Delete the last character
%%%   CTRL-d       - Quit application
%%%
%%% Note:
%%%   This is a simple single-line editor. Multi-line support and
%%%   cursor movement are left as exercises.
%%% @end
%%%-------------------------------------------------------------------
%% Run this example with:
%% make editor

-behavior(cellium).
-include("cellium.hrl").

-export([init/1,update/2, render/1, start/0]).


start() ->
   cellium:start(#{module => ?MODULE}).

init(_Context) ->
    {ok, ""}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, backspace_key} ->
            case length(Model) of
                0 -> "";
                Length -> string:substr(Model, 1, Length - 1)
            end;
        {key, _, _, _, _, <<" ">>} ->
            Model ++ " ";
        {key, _, _, _, _, Key} when is_binary(Key) ->
            Model ++ binary_to_list(Key);
        _ ->
            Model
    end.

render(Text) ->
    Label = io_lib:bformat("> ~s" , [Text]),

    #{type => container,
      id => main_container,
      orientation => horizontal,
      children => [
                   #{type => widget,
                     widget_type => text,
                     id => demo1,
                     value => Label }]}.
