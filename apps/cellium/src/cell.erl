%%%-------------------------------------------------------------------
%%% @author Wade Mealing <wmealing@gmail.com>
%%% @copyright (C) 2025, Wade Mealing
%%% Created : 26 Sep 2025 by Wade Mealing <wmealing@gmail.com>
%%%-------------------------------------------------------------------
-module(cell).
-moduledoc "Represents the most basic drawable unit in Cellium: a single character cell on the terminal.\n\nIt's a fundamental widget used for constructing more complex UI elements. It holds a\ncharacter and its associated foreground and background colors.".

%% API
-export([render/1, new/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%%%===================================================================
%%% API
%%%===================================================================
-doc "Creates a new cell widget.".
-spec new(Id :: any(), Char :: char() | binary()) -> map().
new(Id, Char) ->
    Width = case Char of
        C when is_integer(C) -> 1;
        C when is_binary(C) -> bit_size(C)
    end,
    (widget:new())#{id => Id,
                    type => widget,
                    widget_type => cell,
                    value => Char,
                    width => Width,
                    height => 1
}.

-doc "Renders the cell onto the terminal screen.\n\nIt retrieves the cell's position, character, and colors from the `Widget` map and\nthen uses the underlying termbox library to draw it.".
-spec render(map()) -> ok.
render(Widget) ->
    #{x := X,
      y := Y,
      fg := Fg,
      bg := Bg} = get_common_props(Widget),

    Ch = maps:get(value, Widget, <<"x">> ),

    ?TERMBOX:tb_set_cell(X,Y,Ch,Fg,Bg).
