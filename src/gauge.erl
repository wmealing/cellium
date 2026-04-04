-module(gauge).
-moduledoc """
Gauge widget module for displaying labeled value indicators.

This module provides a gauge widget that displays a label followed by a
visual bar showing a value from 0 to 100. Commonly used for volume controls,
levels, or other percentage-based indicators.

## Usage

Basic gauge:
```
gauge:new(volume_gauge)
```

Gauge with label and value:
```
Widget = gauge:new(volume_gauge),
CustomWidget = Widget#{label => <<"Volume">>, value => 75, width => 30}
```

## Properties

- `value` (integer): Current value from 0 to 100. Default: 0
- `label` (binary): Text label displayed before the gauge bar. Default: empty
- `width` (integer): Total width including label and bar. Set by layout
- `focusable` (boolean): Set to true by default

## Display

The gauge displays as: `Label ████████░░░░`
- `█` for filled portion (proportional to value)
- `░` for unfilled portion
- The bar width is calculated as: `width - label_length - 1`

When focused, foreground and background colors are swapped.

## Example

```
% Volume at 50% with width of 20
Widget#{label => <<"Vol">>, value => 50, width => 20}
% Displays: Vol ████████░░░░░░░
```
""".

-export([render/2, render_focused/2, new/1, handle_event/2]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc """
Creates a new gauge with default settings.

Default value is 0 and label is empty.
""".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => gauge,
                    type => widget,
                    value => 0,
                    label => <<>>,
                    focusable => true}.

-doc "Handles keyboard events for the gauge. Adjusts value with Left/Right arrow keys.".
-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, left_key}, State) ->
    Value = maps:get(value, State, 0),
    State#{value => max(0, Value - 5)};
handle_event({key, _, _, _, _, right_key}, State) ->
    Value = maps:get(value, State, 0),
    State#{value => min(100, Value + 5)};
handle_event(_, State) ->
    State.

-doc "Renders the gauge in unfocused state.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    render_with_colors(Widget, false, Buffer).

-doc "Renders the gauge in focused state with inverted colors.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    render_with_colors(Widget, true, Buffer).

render_with_colors(Widget, Focused, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    {FinalFg, FinalBg} = case Focused of
        true -> {Bg, Fg};
        false -> {Fg, Bg}
    end,
    Value = maps:get(value, Widget, 0),
    Label = maps:get(label, Widget, <<>>),
    Width = min(maps:get(width, Widget, 10), maps:get(requested_width, Widget, maps:get(width, Widget, 10))),

    Buffer1 = cellium_buffer:put_string(X, Y, FinalFg, FinalBg, Label, Buffer),

    % Render progress bar
    BarWidth = Width - byte_size(Label) - 1,
    if
        BarWidth > 0 ->
            Filled = trunc(BarWidth * Value / 100),
            Buffer2 = lists:foldl(fun(I, Acc) ->
                cellium_buffer:set_cell(X + byte_size(Label) + 1 + I, Y, 16#2588, FinalFg, FinalBg, Acc) %% █
            end, Buffer1, lists:seq(0, max(0, Filled - 1))),
            lists:foldl(fun(I, Acc) ->
                cellium_buffer:set_cell(X + byte_size(Label) + 1 + I, Y, 16#2591, FinalFg, FinalBg, Acc) %% ░
            end, Buffer2, lists:seq(Filled, max(Filled, BarWidth - 1)));
        true ->
            Buffer1
    end.
