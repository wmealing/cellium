%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(spinner).
-moduledoc """
Spinner widget module for displaying loading/activity indicators.

This module provides an animated spinner widget that cycles through a series
of frames to indicate ongoing activity or loading state.

## Usage

Basic spinner:
```
spinner:new(loading_spinner)
```

Spinner with custom frames:
```
Widget = spinner:new(loading_spinner),
CustomWidget = Widget#{frames => ["-", "\\", "|", "/"]}
```

## Properties

- `frame` (integer): Current frame index. Increment this in your update loop
  to animate the spinner. Default: 0
- `frames` (list of strings): List of characters/strings to cycle through.
  Default: `["⣷", "⣯", "⣟", "⡿", "⢿", "⣻", "⣽", "⣾"]` (Braille dots)
- `focusable` (boolean): Not focusable by default

## Animation

To animate the spinner, increment the frame value in your update function:

```
update(Model, tick) ->
    erlang:send_after(100, self(), tick),
    Model#{spinner_frame => maps:get(spinner_frame, Model) + 1}.
```

Then pass the frame to the widget:
```
{spinner, [{id, s1}, {frame, maps:get(spinner_frame, Model)}]}
```

## Default Frames

The default frames use Unicode Braille patterns for a smooth circular
animation. Alternative classic frames: `["|", "/", "-", "\\"]`
""".

-export([render/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc """
Creates a new spinner with default Braille dot animation frames.

The spinner starts at frame 0. Increment the frame value to animate.
""".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => spinner,
                    frame => 0,
                    frames => ["⣷", "⣯", "⣟", "⡿", "⢿", "⣻","⣽","⣾"],
                    type => widget}.

-doc """
Renders the current frame of the spinner.

The frame index wraps around using modulo, so incrementing indefinitely works.
""".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    FrameIdx = maps:get(frame, Widget, 0),
    Frames = maps:get(frames, Widget, ["|", "/", "-", "\\"]),
    Frame = lists:nth((FrameIdx rem length(Frames)) + 1, Frames),
    cellium_buffer:put_string(X, Y, Fg, Bg, Frame, Buffer).
