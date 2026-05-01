-module(link_helper).
-export([make_link/2]).

make_link(Url, Label) ->
    %% \033 is the octal code for Escape
    OSC = "\033]8;;",
    ST  = "\033\\",
    OSC ++ Url ++ ST ++ Label ++ OSC ++ ST.
