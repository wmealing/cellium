-module(widgets_gallery).

-behavior(cellium).
-include("cellium.hrl").

-export([init/1, render/1, update/2, start/0]).

init(_Args) ->
    ListItems = [
        "Item 01 - Erlang/OTP",
        "Item 02 - Elixir",
        "Item 03 - Gleam",
        "Item 04 - LFE (Lisp Flavoured Erlang)",
        "Item 05 - Alpaca",
        "Item 06 - Efene",
        "Item 07 - Joxa",
        "Item 08 - Clojerl",
        "Item 09 - Erlog",
        "Item 10 - Hamler",
        "Item 11 - Purerl",
        "Item 12 - Caramel",
        "Item 13 - Reia",
        "Item 14 - Elchemy",
        "Item 15 - Lumen",
        "Item 16 - AtomVM",
        "Item 17 - Enigma",
        "Item 18 - Geoerl",
        "Item 19 - Erlang.js",
        "Item 20 - WebBEAM"
    ],
    Model = #{
        selected_option  => a,
        rg_colour        => red,
        rg_size          => small,
        spinner_frame    => 0,
        focused_id       => undefined,
        active_tab       => 0,
        list_items       => ListItems,
        widget_states => #{
            ti1 => #{text => "Edit me!", cursor_pos => 8},
            li1 => #{items => ListItems, selected_index => 0, scroll_offset => 0},
            g1  => #{value => 50, label => <<"Volume">>},
            pb1 => #{progress => 0.3},
            cb1 => #{checked => true},
            cb2 => #{checked => false},
            edit_tbl => #{
                rows => [
                    ["Alice", "28", "Engineer"],
                    ["Bob",   "35", "Designer"],
                    ["Carol", "42", "Manager"]
                ],
                selected_row => 0,
                selected_col => 0,
                editing      => false
            }
        }
    },
    erlang:send_after(100, self(), tick),
    {ok, Model}.

update(Model, Msg) ->
    case Msg of
        {key, _, _, _, _, <<"q">>} ->
            cellium:stop(),
            Model;
        %% Shift + Right Arrow — next tab (8 tabs now)
        {key, true, false, false, false, right_key} ->
            Active = maps:get(active_tab, Model, 0),
            Model#{active_tab => (Active + 1) rem 8};
        %% Shift + Left Arrow — previous tab
        {key, true, false, false, false, left_key} ->
            Active = maps:get(active_tab, Model, 0),
            Model#{active_tab => (Active + 7) rem 8};
        tick ->
            erlang:send_after(100, self(), tick),
            Model#{spinner_frame => maps:get(spinner_frame, Model) + 1};
        {focus_changed, #{to := NewFocusedId}} ->
            Model#{focused_id => NewFocusedId};
        {radiogroup_changed, rg_options, Value} ->
            Model#{selected_option => Value};
        {radiogroup_changed, rg_colour, Value} ->
            Model#{rg_colour => Value};
        {radiogroup_changed, rg_size, Value} ->
            Model#{rg_size => Value};
        {button_clicked, btn1} ->
            logger:info("Submit clicked!"),
            Model;
        {button_clicked, btn2} ->
            logger:info("Cancel clicked!"),
            Model;
        _ ->
            Model
    end.

render(Model) ->
    FocusedId  = maps:get(focused_id, Model, undefined),
    ActiveTab  = maps:get(active_tab, Model, 0),
    StatusText = io_lib:format("Shift+Arrows: Tabs | Q: Quit | Focused: ~p", [FocusedId]),

    {vbox, [{id, main}, {padding, 0}], [
        {tabs,
            [{id, gallery_tabs},
             {tabs, ["Basic", "Progress", "Input", "Dropdown", "List", "Table", "Edit Table", "Radio Group", "Memory"]},
             {active_tab, ActiveTab},
             {expand, true}],
            [
                tab_basic(Model),
                tab_progress(Model),
                tab_input(),
                tab_dropdown(Model),
                tab_list(),
                tab_table(),
                tab_edit_table(),
                tab_radiogroup(Model),
                tab_memory()
            ]},
        {status_bar, [{id, sb1}, {color, white}], lists:flatten(StatusText)}
    ]}.

%% -------------------------------------------------------------------------
%% Tab content helpers
%% -------------------------------------------------------------------------

tab_basic(Model) ->
    {vbox, [{id, tab_basic}, {expand, true}, {padding, 1}], [
        {header, [{id, h1}, {color, cyan}], "Basic Controls"},
        {spacer, [{size, 1}]},

        {hbox, [{id, row1}, {size, 5}], [
            {vbox, [{id, col1}, {expand, true}], [
                {text,  [{id, t1}], "Radio Options:"},
                {radiogroup, [{id, rg_options}, {selected, maps:get(selected_option, Model)}, {size, 2}],
                    ["Option A", "Option B"]}
            ]},
            {vbox, [{id, col2}, {expand, true}], [
                {text,     [{id, t_cb}], "Features:"},
                {checkbox, [{id, cb1}],  "Enable Feature"},
                {checkbox, [{id, cb2}],  "Show Advanced"}
            ]}
        ]},

        {spacer, [{size, 1}]},

        {hbox, [{id, row2}, {size, 1}], [
            {button, [{id, btn1}, {color, green}, {size, 12}], "Submit"},
            {spacer, [{size, 2}]},
            {button, [{id, btn2}, {color, red},   {size, 12}], "Cancel"}
        ]},
        {spacer, [{expand, true}]}
    ]}.

tab_progress(Model) ->
    {vbox, [{id, tab_progress}, {expand, true}, {padding, 1}], [
        {header, [{id, h2}, {color, green}], "Progress & Status"},
        {spacer, [{size, 1}]},
        {vbox, [{id, row3}, {size, 5}], [
            {text, [{id, t2}], "Progress & Status indicators:"},
            {progress_bar, [{id, pb1}, {width, 40}]},
            {gauge,        [{id, g1},  {width, 40}]},
            {hbox, [{id, row3b}], [
                {text,    [{id, t3},  {size, 9}], "Loading: "},
                {spinner, [{id, s1},  {frame, maps:get(spinner_frame, Model)}]},
                {spacer,  [{size, 5}]},
                {text,    [{id, t4},  {size, 8}], "Switch: "},
                {toggle,  [{id, tg1}, {size, 8}]}
            ]}
        ]},
        {spacer, [{expand, true}]}
    ]}.

tab_input() ->
    {vbox, [{id, tab_input}, {expand, true}, {padding, 1}], [
        {header, [{id, h3}, {color, yellow}], "Complex Layouts"},
        {spacer, [{size, 1}]},
        {vbox, [{id, row4}, {expand, true}], [
            {text, [{id, t5}], "Input Field inside a Frame:"},
            {frame, [{id, f_input}, {title, "Input Frame"}, {size, 8}, {color, yellow}], [
                {text_input, [{id, ti1}, {wrap, true}, {expand, true}]}
            ]},
            {spacer, [{size, 1}]}
        ]},
        {spacer, [{expand, true}]}
    ]}.

tab_dropdown(Model) ->
    Selected = maps:get(gallery_selected, Model, "None"),
    {vbox, [{id, tab_dropdown}, {expand, true}, {padding, 1}], [
        {header, [{id, h_dropdown}, {color, cyan}], "Dropdown (Select) Widget"},
        {spacer, [{size, 1}]},
        {text, [], "The dropdown below draws over other widgets when opened:"},
        {spacer, [{size, 1}]},
        {select, [{id, gallery_sel}, {options, ["Red", "Green", "Blue", "Yellow", "Cyan", "Magenta"]}]},
        {spacer, [{size, 2}]},
        {text, [], lists:flatten(io_lib:format("Selected color: ~p", [Selected]))},
        {spacer, [{size, 1}]},
        {frame, [{title, "Overlapped Frame"}, {expand, true}], [
            {text, [], "This frame will be drawn behind the dropdown list."}
        ]}
    ]}.

tab_list() ->
    {vbox, [{id, tab_list}, {expand, true}, {padding, 1}], [
        {header, [{id, h4}, {color, magenta}], "Interactive List"},
        {spacer, [{size, 1}]},
        {frame, [{id, f_list}, {title, "BEAM Languages"}, {expand, true}], [
            {list, [{id, li1}, {expand, true}]}
        ]}
    ]}.

tab_table() ->
    {vbox, [{id, tab_table}, {expand, true}, {padding, 1}], [
        {header, [{id, h5}, {color, cyan}], "Table Widget"},
        {spacer, [{size, 1}]},
        {text,   [{id, t6}], "Example table with headers and data:"},
        {spacer, [{size, 1}]},
        {table,  [{id, tbl1},
                  {style, double},
                  {size, 8},
                  {headers, ["Language", "Year", "Type"]},
                  {column_widths, [20, 8, 12]},
                  {rows, [
                      ["Erlang", "1986", "Functional"],
                      ["Elixir", "2011", "Functional"],
                      ["Gleam",  "2019", "Functional"],
                      ["LFE",    "2008", "Functional"]
                  ]}]},
        {spacer, [{expand, true}]}
    ]}.

tab_edit_table() ->
    {vbox, [{id, tab_edit_table}, {expand, true}, {padding, 1}], [
        {header, [{id, h6}, {color, green}], "Editable Table"},
        {spacer, [{size, 1}]},
        {text,   [{id, t7}], "Arrow keys to navigate, Enter to edit, Tab to next cell:"},
        {spacer, [{size, 1}]},
        {table,  [{id, edit_tbl},
                  {style, rounded},
                  {editable, true},
                  {focusable, true},
                  {size, 6},
                  {headers, ["Name", "Age", "Role"]},
                  {column_widths, [20, 8, 15]}]},
        {spacer, [{expand, true}]}
    ]}.

tab_radiogroup(Model) ->
    Colour = maps:get(rg_colour, Model, red),
    Size   = maps:get(rg_size,   Model, small),
    Summary = lists:flatten(
        io_lib:format("Selection: ~s ~s", [atom_to_list(Size), atom_to_list(Colour)])
    ),
    {vbox, [{id, tab_radiogroup}, {expand, true}, {padding, 1}], [
        {header, [{id, h7}, {color, cyan}], "Radio Group Widget"},
        {spacer, [{size, 1}]},

        {hbox, [{id, rg_row}, {size, 5}], [
            {vbox, [{id, rg_col1}, {expand, true}], [
                {text, [{id, rg_t1}], "Colour (vertical):"},
                {radiogroup, [{id, rg_colour}, {selected, Colour}, {size, 3}],
                    [red, green, blue]}
            ]},
            {vbox, [{id, rg_col2}, {expand, true}], [
                {text, [{id, rg_t2}], "Size (horizontal):"},
                {radiogroup, [{id, rg_size}, {selected, Size},
                              {orientation, horizontal}, {size, 1}],
                    [small, medium, large]}
            ]}
        ]},

        {spacer, [{size, 1}]},
        {text,   [{id, rg_summary}], Summary},
        {spacer, [{expand, true}]}
    ]}.

tab_memory() ->
    Memory = erlang:memory(),
    Rows = [ [atom_to_list(K), io_lib:format("~.2f MB", [V / (1024 * 1024)])] || {K, V} <- Memory ],
    {vbox, [{id, tab_memory}, {expand, true}, {padding, 1}], [
        {header, [{id, h8}, {color, cyan}], "Memory Usage Stats"},
        {spacer, [{size, 1}]},
        {table,  [{id, mem_tbl},
                  {style, rounded},
                  {size, 12},
                  {headers, ["Category", "Usage"]},
                  {column_widths, [20, 15]},
                  {rows, Rows}]},
        {spacer, [{expand, true}]}
    ]}.

start() ->
    application:ensure_all_started(cellium),
    cellium:start(#{
        module     => ?MODULE,
        auto_focus => true
    }).
