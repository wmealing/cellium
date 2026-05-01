-module(vbox).
-moduledoc """
Vertical box container for stacking widgets vertically.

vbox is a layout container that arranges its children one after another
from top to bottom. It handles automatic height distribution among
expanding and fixed-size children.

## Usage

DSL:
```
{vbox, [{id, main_layout}, {padding, 1}], [
    {header, [], "Title"},
    {text, [], "Content goes here"},
    {button, [{id, ok_btn}], "OK"}
]}
```

Functional:
```
vbox:new(main_layout)
```
""".

-export([new/1, new/2, render/2, render_focused/2]).

-doc "Creates a new vertical box container.".
-spec new(term()) -> map().
new(Id) ->
    container:new(Id, vertical).

-doc "Creates a new vertical box container with children.".
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
