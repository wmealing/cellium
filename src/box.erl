%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(box).
-moduledoc """
Box widget module for rendering rectangular containers with borders.

This module provides a container widget that draws a border around its
contents. The box can contain child widgets and renders with different
border styles depending on focus state.

## Usage

Basic box as a container:
```
{box, [{id, my_box}, {size, 10}, {color, yellow}], [
    {text_input, [{id, input1}, {expand, true}]}
]}
```

Empty decorative box:
```
box:new(decorative_box, 20, 5)
```

## Properties

- `width` (integer): Width of the box in characters. Set by layout when used
  as a container
- `height` (integer): Height of the box in characters. Set by layout when used
  as a container
- `orientation` (atom): Layout orientation for children. Default: vertical
- `padding` (map): Inner padding. Default: 1 on all sides
- `type`: Set to `container` (can hold child widgets)
- `color` (atom): Border color

## Border Styles

- **Unfocused**: Square/single-line border (`┌─┐│└┘`)
- **Focused or child has focus**: Double-line border (`╔═╗║╚╝`)

## Container Behavior

The box is a container widget, so:
- It can have children specified in the DSL
- The layout system calculates dimensions for children
- Default padding of 1 character on all sides
- Children are laid out vertically by default

## Example

```
{box, [{id, input_box}, {size, 5}, {color, cyan}], [
    {text_input, [{id, ti1}, {wrap, true}, {expand, true}]}
]}
```
""".

% API
-export([render/2, new/1, new/3]).
-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new box with default dimensions (0x0).".
-spec new(term()) -> map().
new(Id) ->
    new(Id, 0, 0).

-doc """
Creates a new box with specified dimensions.

The box is a container that can hold child widgets. Children are laid out
vertically with 1 character padding on all sides.
""".
-spec new(term(), non_neg_integer(), non_neg_integer()) -> map().
new(Id, Width, Height) ->
    (widget:new())#{id => Id,
                    widget_type => box,
                    width => Width,
                    height => Height,
                    orientation => vertical,
                    padding => #{top => 1, bottom => 1, left => 1, right => 1},
                    type => container }.

-doc """
Renders the box border.

Border style changes based on focus:
- Focused or child has focus: double-line border
- Unfocused: square/single-line border
""".

-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    HasFocus = maps:get(has_focus, Widget, false),
    HasChildFocus = has_child_focus(Widget),

    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),

    BoxStyle =
        case HasFocus orelse HasChildFocus of
            true ->
                box_styles:double();
            false ->
                box_styles:square()
        end,

    Width = min(maps:get(width, Widget, 0), maps:get(requested_width, Widget, maps:get(width, Widget, 0))),
    Height = min(maps:get(height, Widget, 0), maps:get(requested_height, Widget, maps:get(height, Widget, 0))),

    if
        Height > 0 andalso Width > 0 ->
            box_styles:render_box(X, Y, Width, Height, BoxStyle, "", left, Fg, Bg, Buffer);
        true ->
            Buffer
    end.


has_child_focus(#{children := Children}) ->
    lists:any(fun(Child) -> 
        maps:get(focused, Child, false) orelse has_child_focus(Child)
    end, Children);
has_child_focus(_) ->
    false.
