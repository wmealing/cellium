-module(dialog).
-moduledoc """
Dialog widget module for floating, centered containers.

A dialog is a specialized container that defaults to being centered on the
screen (or its parent). It draws a border and can have a title, similar to a frame.

## Usage

```
{dialog, [{id, my_dialog}, {title, "Confirm"}, {width, 40}, {height, 10}], [
    {text, [], "Are you sure?"},
    {hbox, [], [
        {button, [{id, yes_btn}], "Yes"},
        {button, [{id, no_btn}], "No"}
    ]}
]}
```

## Properties

- `width` (integer): Width of the dialog. Default: 40
- `height` (integer): Height of the dialog. Default: 10
- `title` (string): Title displayed in the top border.
- `position` (atom): Defaults to `centered`.
""".

-export([new/1, new/3, render/2]).
-include("cellium.hrl").

-doc "Creates a new dialog container with centered position.".
-spec new(term()) -> map().
new(Id) ->
    new(Id, 40, 10).

-doc "Creates a new dialog container with specified width and height.".
-spec new(term(), integer(), integer()) -> map().
new(Id, Width, Height) ->
    (frame:new(Id, Width, Height))#{
        widget_type => dialog,
        position => centered
    }.

-doc "Renders the dialog using the frame rendering logic.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    frame:render(Widget, Buffer).
