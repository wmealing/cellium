.PHONY: test go funky

test:
	rebar3 shell --eval "demo:simple()."

go:
	erl -noinput -pa _build/default/lib/*/ebin -pa _checkouts/termbox2_nif/_build/default/lib/termbox2_nif/ebin  -eval "demo:go()"

funky:
	erl -eval 'demo:go()' -pa /Users/wmealing/Projects/erlang/cellium/_build/default/checkouts/termbox2_nif/ebin -pa /Users/wmealing/Projects/erlang/cellium/_build/default/lib/maps_in/ebin -pa /Users/wmealing/Projects/erlang/cellium/_build/default/lib/cellium/ebin  -noshell
