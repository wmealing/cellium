-module(huenicorn).

-import(model, [maybe_set_focus/1]).
-export([start/0]).
-export([init/1, update/2, render/1]).

-include_lib("cellium.hrl").

start() ->
   cellium:start(#{module => ?MODULE,
                   color_type => truecolor,
                   auto_focus => true}).

label_and_input(IdAtom, LabelTextBinary)  ->
    Label = (text:new(make_ref(), LabelTextBinary))#{size => 15,
                                                     color => "FFFFFF"},
    Field = (text:new(IdAtom, <<"_________________">>))#{size => 20,
                                                         color => "000000"},

    (container:new(make_ref(), horizontal))#{ size => 1,
                                              color => "0A0AFF",
                                              'background-color' => "00FF00",
                                              debug => true,
                                              children => [
                                                          Label,
                                                          Field
                                                         ]}.

checkbox_for(Color) ->
    NameRef = make_ref(),
    focus_manager:register_widget(NameRef),
    (checkbox:new(NameRef, Color))#{size => 1, color => "fe7f13"}.


page_one() ->

    Frames = (container:new(make_ref(), 
                            horizontal))#{expand   => true,
                                          children => [
                                                       primary_colors_frame(),
                                                       best_colors_frame()
                                                      ]},

    FwdButton = (button:new(make_ref(), <<"Forward →"/utf8>>))#{expand => true},
    BackButton = (button:new(make_ref(), <<"← Back"/utf8>>))#{expand => true},

    ButtonContainer = (container:new(make_ref(),
                                     horizontal))#{size => 3,
                                                   children => [BackButton, FwdButton]},
    (container:new(make_ref(),
                   vertical))#{expand   => true,
                               children => [Frames, ButtonContainer]}.


best_colors_frame() ->

    BestColors = [ red,
                   yellow_red,
                   yellow,
                   green_yellow,
                   green,
                   blue_green,
                   blue,
                   purple_blue,
                   purple,
                   red_purple ],

    BestColorsCheckboxes  =
        [ checkbox_for(atom_to_binary(Color)) || Color <:- BestColors],

    BCFrame = (frame:new(baz))#{children => BestColorsCheckboxes,
                                orientation => vertical,
                                expand => true,
                                padding => #{top => 1,
                                             bottom => 1,
                                             left => 3,
                                             right => 1},
                                text => <<"[ Best colors ]">> },
    BCFrame.




primary_colors_frame() ->
    PrimaryColors = [
                 spring_vivid,  spring_light,
                 spring_pale,   summer_pale,
                 summer_whitish,summer_soft,
                 autumn_soft,   autumn_dull,
                 autumn_deep,   winter_deep,
                 winter_dark,   winter_vivid ],

%%    CustomerName = label_and_input(foo1, <<"Customer Name: ">>),
%%    CustomerEmail = label_and_input(foo1, <<"Customer Email: ">>),

    PrimaryColorCheckboxes  =
        [ checkbox_for(atom_to_binary(Color)) || Color <:- PrimaryColors],

    PCContainer = (frame:new(baz))#{children => PrimaryColorCheckboxes,
                                    orientation => vertical,
                                    expand => true, 
                                    padding => #{top => 1,
                                                 bottom => 1,
                                                 left => 3,
                                                 right => 1},
                                    text => <<"[ Primary colors ]">> },

    (container:new(page_one_container,
                   vertical))#{ expand => true,
                                color => "0A0AFF",
                                'background-color' => "00AA00",
                                children => [PCContainer] }.

init(_Ignored) ->
    WidgetList = [box1, box2, box3, box4, box5, box6],

    lists:map(fun(W) -> 
                      focus_manager:register_widget(W)
              end, WidgetList),

    {ok, #{}}.

update(Model, Msg) ->
  case Msg of
         {key,_ ,_ ,_ ,_,<<"q">>} ->
          cellium:stop(),
          Model;
      {key,_,_,_,_, <<"i">>} ->
          io:format("FOCUS LIST: ~p",
                    [focus_manager:list_all()]),

          logger:info("WUT");
      {key,_ ,_ ,_ ,_,tab_key} -> 
          logger:info("TAB KEY PRESSED");
       _Else ->
          Model
  end.

render(_Model) ->
    S = page_one(),
    S.



