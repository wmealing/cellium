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

counter:
	erl -sname node1 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'counter:start()'

editor:
	erl -sname node1 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'editor:start()'




