-module(toggle).
-moduledoc """
Toggle widget module for on/off switches.

Toggles are focusable widgets that can be switched with Enter or Space.
They display as `< ON >` or `< OFF >`.

## Usage

```
toggle:new(feature_toggle)
```

## Properties

- `on` (boolean): Whether the toggle is on. Default: false
- `focusable` (boolean): Always true for toggles
- `focused` (boolean): Set by focus manager when toggle has focus

## Rendering

Format: `< ON >` or `< OFF >`
When focused, colors are inverted to highlight the toggle.
""".

-export([render/2, render_focused/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new toggle widget in the OFF state.".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => toggle,
                    on => false,
                    focusable => true,
                    type => widget}.

-doc "Renders the toggle widget to the buffer.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    On = maps:get(on, Widget, false),
    Status = case On of true -> "ON "; false -> "OFF" end,
    cellium_buffer:put_string(X, Y, Fg, Bg, "< " ++ Status ++ " >", Buffer).

-doc "Renders the toggle in focused state with inverted colors.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    On = maps:get(on, Widget, false),
    Status = case On of true -> "ON "; false -> "OFF" end,
    cellium_buffer:put_string(X, Y, Bg, Fg, "< " ++ Status ++ " >", Buffer).
