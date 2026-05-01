-module(link_helper).
-export([make_link/2]).

make_link(Url, Label) ->
    %% \x1b is ESC (27)
    %% \x5c is \   (92)
    OSC = "\x1b]8;;",
    ST  = "\x1b\x5c", 
    OSC ++ Url ++ ST ++ Label ++ OSC ++ ST.
