%%%-------------------------------------------------------------------
%%% @doc
%%% Dual editor example demonstrating the focus_manager.
%%%
%%% This example shows:
%%% - Two independent text editor widgets
%%% - Using focus_manager to switch between them with Tab/Shift+Tab
%%% - Visual indication of which editor has focus
%%% - Independent state management for each editor
%%%
%%% Usage:
%%%   dual_editor_nofocus:start()
%%%
%%% Controls:
%%%   Type text       - Input characters into focused editor
%%%   Backspace       - Delete last character in focused editor
%%%   Tab             - Move focus to the next editor
%%%   Shift+Tab       - Move focus to the previous editor
%%%   CTRL-d          - Quit application
%%%
%%% The example demonstrates:
%%% 1. Registering multiple widgets with focus_manager
%%% 2. Handling focus change events
%%% 3. Visual feedback showing which editor is active
%%% @end
%%%-------------------------------------------------------------------

-module(dual_editor_nofocus).

-behavior(cellium).
-include("cellium.hrl").

-export([init/1, update/2, render/1, start/0]).

start() ->
    cellium:start(#{module=>?MODULE}).

%% Initialize with two empty editors and focus on the first
init(_Context) ->
    register_editors(),
    {ok, #{
        editor1 => [<<"">>],
        editor2 => [<<"">>],
        focused => editor1
    }}.

%% Register both editors with the focus_manager
register_editors() ->
    focus_manager:register_widget(editor1),
    focus_manager:register_widget(editor2).


%% Update the model based on user input or focus changes
update(Model, Msg) ->
    {ok, FocusedWidget} = focus_manager:get_focused(),
    TextData  = maps:get(FocusedWidget, Model),
    Key = get_key_code(Msg),
    NewText = apply_key_to_text(Key, TextData),
    maps:update(FocusedWidget, NewText, Model).

%% Apply a keycode to text (add character or delete on backspace)
apply_key_to_text({127, 0}, Text) ->
    delete_last_char(Text);
apply_key_to_text({_, 32}, Text) ->
    Text ++ " ";
apply_key_to_text({_, 0}, Text) ->
    Text;
apply_key_to_text({_, Num}, Text) when is_integer(Num) ->
    lists:flatten([Text, [Num]]).

%% Delete the last character from text
delete_last_char(Text) ->
    Length = length(Text),
    case Length > 0 of
        true -> string:substr(Text, 1, Length - 1);
        false -> Text
    end.

%% Render the UI with both editors and focus indicator
render(#{editor1 := Text1, editor2 := Text2}) ->
    Label1 = build_editor_label("Editor 1", Text1, editor1),
    Label2 = build_editor_label("Editor 2", Text2, editor2),

    #{type => container,
      id => main_container,
      orientation => vertical,
      children => [
        #{type => widget,
          widget_type => text,
          id => title,
          size => 1,
          value => <<"Dual Widget Editor - Use Tab to switch focus">>},

        #{type => widget,
          widget_type => text,
          id => empty_line_1,
          size => 1,
          value => <<"">>},

        #{type => widget,
          widget_type => text,
          id => editor1_label,
          size => 1,
          value => Label1},

        #{type => widget,
          widget_type => text,
          id => empty_line_2,
          size => 1,
          value => <<"">>},

        #{type => widget,
          widget_type => text,
          id => editor2_label,
          size => 1,
          value => Label2},

        #{type => widget,
          widget_type => text,
          id => help,
          size => 1,
          value => <<"[Tab] Next  [Shift+Tab] Prev  [Ctrl-D] Quit">>}
      ]}.

%% Build a label for an editor showing focus status
build_editor_label(Title, Content, EditorId) ->
    case focus_manager:get_focused() of 
	{ok, EditorId} ->
    		io_lib:bformat("~s ~s: ~s", [">>", Title, Content]);
	_Other ->
    		io_lib:bformat("~s ~s: ~s", ["  ", Title, Content])
	end.


get_key_code({tb_event, key, _, {keydata, Code1, Code2}}) ->
    {Code1, Code2}.
