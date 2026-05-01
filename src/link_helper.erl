-module(link_helper).
-export([make_link/2]).

make_link(Url, Label) ->
    %% \e]8;; is the start
    %% \e\\ is the terminator (ST)
    Prefix = "\e]8;;" ++ Url ++ "\e\\",
    Suffix = "\e]8;;\e\\",
    Prefix ++ Label ++ Suffix.
