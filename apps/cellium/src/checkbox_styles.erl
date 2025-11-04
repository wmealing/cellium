%%% @doc Checkbox styles module for rendering checkbox indicators.
%%%
%%% This module provides different visual styles for checkboxes in both
%%% ASCII and UTF-8 formats. Styles can be selected based on terminal
%%% capabilities and user preference.
%%% @end
-module(checkbox_styles).
-export([get_style/2]).

%%% @doc Returns the visual representation of a checkbox.
%%%
%%% Provides checkbox characters for both checked and unchecked states
%%% in either ASCII or UTF-8 encoding. ASCII style is suitable for
%%% terminals with limited Unicode support.
%%%
%%% Styles:
%%% - ASCII unchecked: `[ ]'
%%% - ASCII checked: `[x]'
%%% - UTF-8 unchecked: ☐ (U+2610 BALLOT BOX)
%%% - UTF-8 checked: ☑ (U+2611 BALLOT BOX WITH CHECK)
%%%
%%% @param Style Style atom: `ascii' or `utf8'
%%% @param Checked Boolean indicating checkbox state
%%% @returns Binary string containing the checkbox visual representation
%%% @end
-spec get_style(ascii | utf8, boolean()) -> binary().
get_style(ascii, false) -> <<"[ ]">>;
get_style(ascii, true) -> <<"[x]">>;
get_style(utf8, false) -> <<"☐">>;
get_style(utf8, true) -> <<"☑">>.
