-module(cellium).

-type model() :: map().

-callback init(Args :: term())  -> {ok, State :: term()} | {error, Reason :: term()}.

-callback render(model) ->  {ok, Something :: term() }.
