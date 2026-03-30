-module(progress_bar).
-moduledoc """
Progress bar widget module for displaying task completion.

This module provides a visual progress bar widget that displays completion
percentage using filled and unfilled block characters.

## Usage

Basic progress bar:
```
progress_bar:new(my_progress)
```

Progress bar with custom width and progress:
```
Widget = progress_bar:new(my_progress),
CustomWidget = Widget#{width => 40, progress => 0.75}
```

## Properties

- `progress` (float): Completion percentage from 0.0 to 1.0. Default: 0.0
- `width` (integer): Total width of the progress bar including brackets. Default: 20
- `focusable` (boolean): Set to true by default

## Display

The progress bar uses Unicode block characters:
- `█` for filled portion (completed)
- `░` for unfilled portion (remaining)
- Format: `[████████░░░░░░░░░░]`

When focused, foreground and background colors are swapped.

## Example

```
% 50% complete with width of 30
Widget#{progress => 0.5, width => 30}
% Displays: [██████████████░░░░░░░░░░░░░░]
```
""".

-export([render/2, render_focused/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc """
Creates a new progress bar with default settings.

Default progress is 0.0 (0%) and default width is 20 characters.
""".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => progress_bar,
                    progress => 0.0, %% 0.0 to 1.0
                    width => 20,
                    focusable => true,
                    type => widget}.

-doc "Renders the progress bar in unfocused state.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    render_with_colors(Widget, false, Buffer).

-doc "Renders the progress bar in focused state with inverted colors.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    render_with_colors(Widget, true, Buffer).

render_with_colors(Widget, Focused, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    {FinalFg, FinalBg} = case Focused of
        true -> {Bg, Fg};
        false -> {Fg, Bg}
    end,
    Width = min(maps:get(width, Widget, 20), maps:get(requested_width, Widget, maps:get(width, Widget, 20))),
    Progress = maps:get(progress, Widget, 0.0),
    BarWidth = max(0, Width - 2),
    Filled = round(Progress * BarWidth),
    Bar = lists:duplicate(Filled, "█") ++ lists:duplicate(BarWidth - Filled, "░"),

    cellium_buffer:put_string(X, Y, FinalFg, FinalBg, "[" ++ Bar ++ "]", Buffer).
