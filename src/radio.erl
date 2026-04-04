-module(radio).
-moduledoc """
Radio button widget module for mutually exclusive options.

This module provides an interactive radio button widget that displays a label
with a selectable button. Radio buttons are typically used in groups where
only one option can be selected at a time.

## Usage

Basic radio button:
```
radio:new(option_a, "Option A")
```

Radio button with selected state:
```
Widget = radio:new(option_a, "Option A"),
SelectedWidget = Widget#{selected => true}
```

## Properties

- `label` (string): Text label displayed next to the radio button
- `selected` (boolean): Whether the radio button is selected. Default: false
- `focusable` (boolean): Set to true by default

## Display

- Unselected: `( ) Label`
- Selected: `(*) Label`
- When focused, foreground and background colors are swapped

## Event Handling

Set the selected state in your update function when handling space or enter
key events. Remember to deselect other radio buttons in the same group.
""".

-export([render/2, render_focused/2, new/1, new/2, handle_event/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new radio button with default label 'Radio'.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, "Radio").

-doc "Creates a new radio button with the specified label.".
-spec new(term(), string()) -> map().
new(Id, Label) ->
    (widget:new())#{id => Id,
                    widget_type => radio,
                    label => Label,
                    selected => false,
                    focusable => true,
                    type => widget}.

-doc "Handles keyboard events for the radio button. Sets selected to true on Space or Enter.".
-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, <<" ">>}, State) ->
    select_radio(State);
handle_event({key, _, _, _, _, enter_key}, State) ->
    select_radio(State);
handle_event(_, State) ->
    State.

select_radio(State) ->
    Id = maps:get(id, State),
    Group = maps:get(group, State, undefined),
    self() ! {radio_selected, Id, Group},
    State#{selected => true}.

-doc "Renders the radio button in unfocused state.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Label = maps:get(label, Widget, ""),
    Selected = maps:get(selected, Widget, false),
    Mark = case Selected of true -> "*"; false -> " " end,
    cellium_buffer:put_string(X, Y, Fg, Bg, "(" ++ Mark ++ ") " ++ Label, Buffer).

-doc "Renders the radio button in focused state with inverted colors.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    Label = maps:get(label, Widget, ""),
    Selected = maps:get(selected, Widget, false),
    Mark = case Selected of true -> "*"; false -> " " end,
    cellium_buffer:put_string(X, Y, Bg, Fg, "(" ++ Mark ++ ") " ++ Label, Buffer).
