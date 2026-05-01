-module(breadcrumbs).
-moduledoc """
Breadcrumbs widget for displaying navigation history.

Renders a horizontal trail of path segments joined by a separator,
for example: `Home > Settings > Network`

## Properties

- `crumbs` (list of strings): The ordered list of path segments. Default: [].
- `separator` (string): The string placed between segments. Default: " > ".
- `focusable` (boolean): Default false.

## Display

Example with crumbs ["Home", "Settings", "Network"] and default separator:
`Home > Settings > Network`
""".

-export([new/1, new/2, render/2, render_focused/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

-doc "Creates a new breadcrumbs widget with an empty crumb list.".
-spec new(term()) -> map().
new(Id) ->
    (widget:new())#{id          => Id,
                    widget_type => breadcrumbs,
                    crumbs      => [],
                    separator   => " > ",
                    focusable   => false,
                    type        => widget}.

-doc "Creates a new breadcrumbs widget with an initial list of crumbs.".
-spec new(term(), list()) -> map().
new(Id, Crumbs) ->
    (new(Id))#{crumbs => Crumbs}.

-doc "Renders the breadcrumbs widget in unfocused state.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    render_crumbs(Widget, Buffer, false).

-doc "Renders the breadcrumbs widget in focused state with inverted colours.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    render_crumbs(Widget, Buffer, true).

render_crumbs(Widget, Buffer, Focused) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    {FinalFg, FinalBg} = choose_colors(Focused, Fg, Bg),
    Crumbs    = maps:get(crumbs,    Widget, []),
    Separator = maps:get(separator, Widget, " > "),
    Line      = build_line(Crumbs, Separator),
    cellium_buffer:put_string(X, Y, FinalFg, FinalBg, Line, Buffer).

choose_colors(true,  Fg, Bg) -> {Bg, Fg};
choose_colors(false, Fg, Bg) -> {Fg, Bg}.

build_line([], _Separator) ->
    "";
build_line(Crumbs, Separator) ->
    join_crumbs(Crumbs, Separator).

join_crumbs([H | T], Separator) ->
    lists:foldl(fun(Crumb, Acc) -> Acc ++ Separator ++ Crumb end, H, T).
