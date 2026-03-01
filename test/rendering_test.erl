-module(rendering_test).
-include_lib("eunit/include/eunit.hrl").
-include("cellium.hrl").

rendering_test_() ->
    {setup,
     fun() ->
             %% Setup: Start the mock terminal and set it as the backend
             {ok, Pid} = termbox_mock:start_link(),
             termbox_wrapper:set_backend(termbox_mock),
             ?TERMBOX:tb_init(),
             Pid
     end,
     fun(Pid) ->
             %% Teardown: Stop the mock terminal
             gen_server:stop(Pid)
     end,
     [
      {"Test simple cell rendering",
       fun() ->
               ?TERMBOX:tb_clear(),
               Cell = cell:new(test_cell, <<"A">>),
               %% Use 'color' and 'background-color' to override defaults
               CellWithPos = Cell#{x => 10, y => 5, color => ?TB_WHITE, 'background-color' => ?TB_BLACK},
               cell:render(CellWithPos),
               
               {Char, Fg, Bg} = termbox_mock:get_cell(10, 5),
               ?assertEqual($A, Char),
               ?assertEqual(?TB_WHITE, Fg),
               ?assertEqual(?TB_BLACK, Bg)
       end},
      {"Test text rendering",
       fun() ->
               ?TERMBOX:tb_clear(),
               TextWidget = text:new(test_text, <<"Hello">>),
               TextWithPos = TextWidget#{x => 0, y => 0, color => ?TB_WHITE, 'background-color' => ?TB_BLACK},
               text:render(TextWithPos),
               
               %% Verify each character
               ?assertEqual({$H, ?TB_WHITE, ?TB_BLACK}, termbox_mock:get_cell(0, 0)),
               ?assertEqual({$e, ?TB_WHITE, ?TB_BLACK}, termbox_mock:get_cell(1, 0)),
               ?assertEqual({$l, ?TB_WHITE, ?TB_BLACK}, termbox_mock:get_cell(2, 0)),
               ?assertEqual({$l, ?TB_WHITE, ?TB_BLACK}, termbox_mock:get_cell(3, 0)),
               ?assertEqual({$o, ?TB_WHITE, ?TB_BLACK}, termbox_mock:get_cell(4, 0))
       end},
      {"Test frame rendering (partial)",
       fun() ->
               ?TERMBOX:tb_clear(),
               Frame = frame:new(test_frame),
               FrameWithPos = Frame#{x => 0, y => 0, width => 10, height => 3, text => <<"Title">>},
               frame:render(FrameWithPos),
               
               %% Double border top left is ╔ (unicode 2554)
               {Char, _, _} = termbox_mock:get_cell(0, 0),
               %% Double border top-left character code
               ?assertEqual(9556, Char), % ╔
               
               %% Check if title is present
               {T, _, _} = termbox_mock:get_cell(2, 0),
               ?assertEqual($T, T)
       end}
     ]}.
