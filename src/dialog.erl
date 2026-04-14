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

-export([new/1, new/3, render/2, render_overlay/2]).
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

-doc "Renders the dialog. In the first pass, we do nothing to avoid clipping.".
-spec render(map(), map()) -> map().
render(_Widget, Buffer) ->
    Buffer.

-doc "Renders the dialog in the overlay pass to draw over the top of other widgets.".
-spec render_overlay(map(), map()) -> map().
render_overlay(Widget, Buffer) ->
    % We use frame:render but we must ensure we don't use the clipped buffer 
    % from the first pass if we want to draw outside parent bounds.
    % Since widgets:render_overlays passes the 'global' buffer, this works.
    ResultBuffer = frame:render(Widget, Buffer),
    
    % Clear clip before rendering children to ensure they are visible within the dialog
    % (frame:render might have set a clip)
    BufferNoClip = cellium_buffer:clear_clip(ResultBuffer),

    % Dialog is a container, so we must also render its children.
    Children = maps:get(children, Widget, []),
    lists:foldl(fun(Child, Acc) ->
        widgets:render(Child, Acc)
    end, BufferNoClip, Children).
