-module(radiogroup).
-moduledoc """
Radio group widget — a labelled group of mutually exclusive radio buttons.

Renders a set of named options as radio buttons. Only one option can be
selected at a time. The group can be laid out horizontally or vertically.

## Usage

```
radiogroup:new(my_group, [opt_a, opt_b, opt_c])
```

## Properties

- `options`     — list of atoms used as option identifiers and display labels
- `selected`    — atom identifying the currently selected option
- `orientation` — `vertical` (default) or `horizontal`
- `focusable`   — true by default

## Display (vertical, opt_b selected)

```
( ) opt_a
(*) opt_b
( ) opt_c
```

## Display (horizontal)

```
( ) opt_a  (*) opt_b  ( ) opt_c
```

## Messages

When the user changes the selection the group sends:

```
{radiogroup_changed, GroupId, NewSelectedOption}
```
""".

-export([new/2, new/3, handle_event/2, render/2, render_focused/2]).

-include("cellium.hrl").
-import(widget, [get_common_props/1]).

%% -------------------------------------------------------------------------
%% Construction
%% -------------------------------------------------------------------------

-doc "Creates a vertical radiogroup with the given options. First option is selected by default.".
-spec new(term(), [atom()]) -> map().
new(Id, Options) ->
    new(Id, Options, vertical).

-doc "Creates a radiogroup with the given options and orientation (`vertical` | `horizontal`).".
-spec new(term(), [atom()], vertical | horizontal) -> map().
new(Id, Options, Orientation) ->
    Default = first_option(Options),
    (widget:new())#{id              => Id,
                    widget_type     => radiogroup,
                    options         => Options,
                    selected        => Default,
                    focused_option  => Default,
                    orientation     => Orientation,
                    focusable       => true,
                    type            => widget}.

%% -------------------------------------------------------------------------
%% Event handling
%% -------------------------------------------------------------------------

-doc "Moves selection or wraps around on arrow / space / enter key events.".
-spec handle_event(term(), map()) -> map().
handle_event({key, _, _, _, _, up_key}, State) ->
    move_focus(State, -1);
handle_event({key, _, _, _, _, left_key}, State) ->
    move_focus(State, -1);
handle_event({key, _, _, _, _, down_key}, State) ->
    move_focus(State, 1);
handle_event({key, _, _, _, _, right_key}, State) ->
    move_focus(State, 1);
handle_event({key, _, _, _, _, <<" ">>}, State) ->
    confirm_selection(State);
handle_event({key, _, _, _, _, enter_key}, State) ->
    confirm_selection(State);
handle_event(_, State) ->
    State.

%% -------------------------------------------------------------------------
%% Rendering
%% -------------------------------------------------------------------------

-doc "Renders the radiogroup in unfocused state.".
-spec render(map(), map()) -> map().
render(Widget, Buffer) ->
    render_group(Widget, Buffer, false).

-doc "Renders the radiogroup with the selected option highlighted.".
-spec render_focused(map(), map()) -> map().
render_focused(Widget, Buffer) ->
    render_group(Widget, Buffer, true).

%% -------------------------------------------------------------------------
%% Internal helpers
%% -------------------------------------------------------------------------

render_group(Widget, Buffer, Focused) ->
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(Widget),
    
    %% Calculate size based on options if not already set
    Options     = maps:get(options, Widget, []),
    Size        = maps:get(size, Widget, length(Options)),
    
    %% Inject size into widget for layout engine
    WidgetWithSize = Widget#{size => Size},
    #{x := X, y := Y, fg := Fg, bg := Bg} = get_common_props(WidgetWithSize),

    Selected    = maps:get(selected, Widget, undefined),
    FocusedOpt  = maps:get(focused_option, Widget, undefined),
    Orientation = maps:get(orientation, Widget, vertical),
    render_options(Options, Selected, FocusedOpt, X, Y, Fg, Bg, Focused, Orientation, Buffer).

render_options([], _, _, _, _, _, _, _, _, Buffer) ->
    Buffer;
render_options([Opt | Rest], Selected, FocusedOpt, X, Y, Fg, Bg, Focused, vertical, Buffer) ->
    B1 = render_one(Opt, Selected, FocusedOpt, X, Y, Fg, Bg, Focused, Buffer),
    render_options(Rest, Selected, FocusedOpt, X, Y + 1, Fg, Bg, Focused, vertical, B1);
render_options([Opt | Rest], Selected, FocusedOpt, X, Y, Fg, Bg, Focused, horizontal, Buffer) ->
    Label  = option_label(Opt),
    Width  = 4 + length(Label) + 2,
    B1     = render_one(Opt, Selected, FocusedOpt, X, Y, Fg, Bg, Focused, Buffer),
    render_options(Rest, Selected, FocusedOpt, X + Width, Y, Fg, Bg, Focused, horizontal, B1).

render_one(Opt, Selected, FocusedOpt, X, Y, Fg, Bg, Focused, Buffer) ->
    Mark  = mark_for(Opt, Selected),
    Label = option_label(Opt),
    Text  = "(" ++ Mark ++ ") " ++ Label,
    IsFocused = is_current(Opt, FocusedOpt),
    %% Use the focused background/foreground if this option is the focused one.
    case Focused andalso IsFocused of
        true  -> cellium_buffer:put_string(X, Y, Bg, Fg, Text, Buffer);
        false -> cellium_buffer:put_string(X, Y, Fg, Bg, Text, Buffer)
    end.

mark_for(Opt, Selected) ->
    case is_current(Opt, Selected) of
        true  -> "*";
        false -> " "
    end.

is_current(Opt, Selected) ->
    Opt =:= Selected.

option_label(Opt) when is_atom(Opt) ->
    atom_to_list(Opt);
option_label(Opt) when is_list(Opt) ->
    Opt.

first_option([First | _]) -> First;
first_option([])           -> undefined.

move_focus(State, Delta) ->
    Options  = maps:get(options, State, []),
    Focused  = maps:get(focused_option, State, undefined),
    Next     = next_option(Options, Focused, Delta),
    State#{focused_option => Next}.

confirm_selection(State) ->
    Focused = maps:get(focused_option, State),
    notify_selection(State#{selected => Focused}).

next_option([], _, _) ->
    undefined;
next_option(Options, Selected, Delta) ->
    Len = length(Options),
    Idx = index_of(Selected, Options, 0),
    NewIdx = (Idx + Delta + Len) rem Len,
    lists:nth(NewIdx + 1, Options).

index_of(_, [], _) -> 0;
index_of(Val, [Val | _], Idx) -> Idx;
index_of(Val, [_ | Rest], Idx) -> index_of(Val, Rest, Idx + 1).

notify_selection(State) ->
    Id       = maps:get(id, State),
    Selected = maps:get(selected, State),
    self() ! {radiogroup_changed, Id, Selected},
    State.
