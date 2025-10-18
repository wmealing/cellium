%% If compiled with {d, TEST, true}
%% This happens automatically during `rebar3 eunit`
-ifdef(TEST).
-define(TRACE(Template, Args), io:format(user, "TRACE ~p Line: ~3..0B " ++ Template, [?MODULE, ?LINE | Args])).
-else.
-define(TRACE(_T, _A), void).
-endif.
