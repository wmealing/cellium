.PHONY: test go funky

boo:
	rebar3 shell --sname node1 --setcookie mysecretcookie 

sh:
	rebar3 shell --sname node1 --setcookie mysecretcookie  --eval 'demo:simple().'

remsh:
	erl -sname node2 -setcookie mysecretcookie -remsh node1

test:
	rebar3 shell --eval "demo:simple()."

go:
	erl --sname node1 --setcookie mysecretcookie -noinput -pa _build/default/lib/*/ebin -pa _checkouts/termbox2_nif/_build/default/lib/termbox2_nif/ebin  

funky:
	erl -sname node1 -setcookie mysecretcookie -noshell  -pa /Users/wmealing/Projects/erlang/cellium/_build/default/checkouts/termbox2_nif/ebin -pa /Users/wmealing/Projects/erlang/cellium/_build/default/lib/maps_in/ebin -pa /Users/wmealing/Projects/erlang/cellium/_build/default/lib/cellium/ebin  -eval 'user_code:start()'
