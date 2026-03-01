-module(todo_app).

-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

rand_atom() ->
     list_to_atom(lists:map(fun(_) -> rand:uniform($z - $a) + $a end, lists:seq(1, 10))).

read_file_content(Filename) ->
    case file:read_file(Filename) of
        {ok, BinaryContent} ->
            BinaryContent;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [Filename, Reason]),
            undefined % Or handle the error as needed
    end.

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, List)),
    file:write_file(Filename, Text).

placeholder_text() ->
    Text = (text:new(placeholder_add, <<"PLACEHOLDER TEXT">>))#{expand => true},
    focus_manager:register_widget(placeholder_add),

    #{type => container,
      id => fafo,
      orientation => vertical,
      expand => true,
      padding =>  #{top => 1, left => 1},
      children => [ Text ] }.


task_entry(Model) ->
    focus_manager:register_widget(add_task_button),
    #{ type => container,
       widget_type => frame,
       class => frame,
       size => 5,
       id => task_entry_box,
       text => <<"| Add your task |">>,
       children => [ placeholder_text(),
                    (button:new(add_task_button, <<"Add">> ))#{size=> 10} ] }.


aligned_thing(Thing) ->
       #{ type => container,
          id => pad_thing,
          expand => true,
          padding => #{ top => 1, left => 1 },
          children => [ Thing ] }.

sample_child(TodoText) ->
    Text = (text:new(todo_text, TodoText))#{expand => true},
    EditButton   = (button:new(edit_button,   <<"Edit">>))#{size => 8},
    DeleteButton = (button:new(delete_button, <<"Delete">>))#{size => 10},

    #{ type => container,
       id => sample_children,
       orientation => horizontal,
       size => 3,
       children => [
                  aligned_thing(Text),
                  EditButton,
                  DeleteButton
                   ] }.

sample_children() ->
    Foo1 = <<"This is the first todo">>,
    Foo2 = <<"This is the second todo">>,

    #{ type => container,
       id => sample_children,
       orientation => vertical,
       expand => true,
       children => [sample_child(Foo1), sample_child(Foo2)] }.


task_list(Model) ->

    CurrentChildren = sample_children(),

    #{ type => container,
       widget_type => frame,
       class => frame,
       expand => true,
       id => task_list_box,
       text => <<"| List of tasks |">>,
       children =>  [CurrentChildren] }.



% Example with nested vertical container
main_layout(Model) ->
    #{
        type => container,
        widget_type => frame,
        class => frame,
        width => 20,
        height => 15,
        id => main_frame,
      %% ╠
      %% "╣
        text => <<"| TODO APP |">>,
        children => [
            (container:new(foocontainer, vertical))#{
                expand => true,
                padding => #{left => 1,right => 1,bottom => 0,top => 1},
                children => [task_entry(Model),
                             task_list(Model)]}
       ]
    }.

start() ->
    cellium:start(#{
        module => ?MODULE,
        auto_focus => true
    }).

init(_Ignored) ->
    {ok, ""}.

update(Model, _Event) ->
    Model.

render(Model) ->
    main_layout(Model).
