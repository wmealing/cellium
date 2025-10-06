.PHONY: test go funky


sh:
	rebar3 shell --sname node1 --setcookie mysecretcookie  --eval 'demo:simple().'

remsh:
	erl -sname node2 -setcookie mysecretcookie -remsh node1

test:
	rebar3 shell --eval "demo:simple()."

go:
	erl -noinput -pa _build/default/lib/*/ebin -pa _checkouts/termbox2_nif/_build/default/lib/termbox2_nif/ebin  -eval "demo:go()"

funky:
	erl  -pa /Users/wmealing/Projects/erlang/cellium/_build/default/checkouts/termbox2_nif/ebin -pa /Users/wmealing/Projects/erlang/cellium/_build/default/lib/maps_in/ebin -pa /Users/wmealing/Projects/erlang/cellium/_build/default/lib/cellium/ebin  --noinput --sname node1 --setcookie mysecretcookie  --eval 'demo:simple().'
