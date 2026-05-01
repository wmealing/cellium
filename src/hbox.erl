-module(hbox).
-moduledoc """
Horizontal box container for stacking widgets horizontally.

hbox is a layout container that arranges its children side by side
from left to right. It handles automatic width distribution among
expanding and fixed-size children.

## Usage

DSL:
```
{hbox, [{id, controls}], [
    {button, [{id, prev_btn}], "Prev"},
    {spacer, [{size, 2}]},
    {button, [{id, next_btn}], "Next"}
]}
```

Functional:
```
hbox:new(controls)
```
""".

-export([new/1, new/2, render/2, render_focused/2]).

-doc "Creates a new horizontal box container.".
-spec new(term()) -> map().
new(Id) ->
    container:new(Id, horizontal).

-doc "Creates a new horizontal box container with children.".
-spec new(term(), list()) -> map().
new(Id, Children) ->
    Widget = new(Id),
    Widget#{children => Children}.

-doc "Delegates rendering to the generic container module.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    container:render(Widget, Buffer).

-doc "Delegates focused rendering to the generic container module.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    container:render_focused(Widget, Buffer).
