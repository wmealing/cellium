
-module(checkbox_styles).
-export([get_style/2]).

-spec get_style(atom(), boolean()) -> binary().
get_style(ascii, false) -> <<"[ ]">>;
get_style(ascii, true) -> <<"[x]">>;
get_style(utf8, false) -> <<"☐">>;
get_style(utf8, true) -> <<"☑">>.
