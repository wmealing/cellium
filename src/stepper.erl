-module(stepper).
-moduledoc """
Stepper widget module for displaying multi-step progress.

Displays a sequence of dots connected by lines, where one dot is "filled" 
to indicate the current active step.

## Properties

- `steps` (integer): Total number of steps. Default: 3.
- `current` (integer): The 0-based index of the current step.
- `connector` (string): The character(s) used between dots. Default: "━".
- `filled_char` (string): The character for the active step. Default: "●".
- `empty_char` (string): The character for inactive steps. Default: "○".
- `focusable` (boolean): Default true.

## Display

Example with 3 steps, current=1: `( ○━●━○ )`
""".

-export([new/1, new/2, render/2, render_focused/2, handle_event/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new stepper widget.".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => stepper,
                    type => widget,
                    steps => 3,
                    current => 0,
                    connector => "━",
                    filled_char => "●",
                    empty_char => "○",
                    focusable => true}.

-doc "Creates a new stepper with a specific number of steps.".
-spec new(term(), integer()) -> map().
new(Id, Steps) ->
    (new(Id))#{steps => Steps}.

-doc "Handles left/right arrow keys to move the current step.".
-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, left_key}, State) ->
    Current = maps:get(current, State, 0),
    State#{current => max(0, Current - 1)};
handle_event({key, _, _, _, _, right_key}, State) ->
    Current = maps:get(current, State, 0),
    Steps = maps:get(steps, State, 3),
    State#{current => min(Steps - 1, Current + 1)};
handle_event(_, State) ->
    State.

-doc "Renders the stepper.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    render_stepper(Widget, Buffer, false).

-doc "Renders the stepper with focus (inverted colors).".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    render_stepper(Widget, Buffer, true).

render_stepper(Widget, Buffer, Focused) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    {FinalFg, FinalBg} = case Focused of
        true -> {Bg, Fg};
        false -> {Fg, Bg}
    end,

    Steps = maps:get(steps, Widget, 3),
    Current = maps:get(current, Widget, 0),
    Connector = maps:get(connector, Widget, "━"),
    Filled = maps:get(filled_char, Widget, "●"),
    Empty = maps:get(empty_char, Widget, "○"),

    % Build string: ( char CONNECTOR char CONNECTOR char )
    StepStrings = [ if I == Current -> Filled; true -> Empty end 
                    || I <- lists:seq(0, Steps - 1) ],
    
    Joined = join_with_connector(StepStrings, Connector),
    FinalStr = "( " ++ Joined ++ " )",
    
    cellium_buffer:put_string(X, Y, FinalFg, FinalBg, FinalStr, Buffer).

join_with_connector([H | T], Connector) ->
    lists:foldl(fun(Next, Acc) -> Acc ++ Connector ++ Next end, H, T);
join_with_connector([], _) -> "".
