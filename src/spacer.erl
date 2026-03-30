-module(spacer).
-moduledoc """
Spacer widget module for adding empty space in layouts.

This module provides an invisible spacer widget used to create gaps between
other widgets in horizontal or vertical layouts.

## Usage

Basic spacer (1 unit):
```
spacer:new(gap1)
```

Spacer with custom size:
```
{spacer, [{id, gap1}, {size, 3}]}
```

## Properties

- `size` (integer): Number of units (characters or lines) to occupy in the layout
- `width` (integer): Default 1, usually overridden by layout
- `height` (integer): Default 1, usually overridden by layout

## Behavior

The spacer renders nothing (doesn't modify the buffer) but occupies space
in the layout. Use it to create:
- Gaps between widgets
- Padding in containers
- Alignment spacing

## Layout Usage

In horizontal layouts, spacer size affects horizontal spacing.
In vertical layouts, spacer size affects vertical spacing.

```
{hbox, [{id, row}], [
    {button, [{id, btn1}], "OK"},
    {spacer, [{size, 5}]},
    {button, [{id, btn2}], "Cancel"}
]}
```
""".

-export([render/2, new/1]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc """
Creates a new spacer widget.

Default size is 1 unit. Use the size property in the layout to control spacing.
""".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id => Id,
                    widget_type => spacer,
                    width => 1,
                    height => 1,
                    type => widget}.

-doc """
Renders the spacer (does nothing, spacer is invisible).

The spacer occupies space in the layout but doesn't draw anything.
""".
-spec render(map(), map()) -> map().
render(_Widget, Buffer) ->
    Buffer.
