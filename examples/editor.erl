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
   cellium:start(?MODULE).

get_code({tb_event,key,_,{keydata, Code1, Code2}}) ->
    {Code1, Code2}.

init(_Context) ->
    {ok, ""}.

update(Model, Msg) ->
    case get_code(Msg) of

        {127, 0} ->
            Length = length(Model),
            string:substr(Model, 1, Length -1);
        {_ , 32} ->
            Model ++ " ";
        { _, 0 } ->
            Model;
        { _, Num}  when is_integer(Num) ->
            Model ++ [Num]
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
