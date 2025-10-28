.PHONY: test go funky

EXTRAS_EBIN = -pa ./_build/default/extras/examples/
export EXTRAS_EBIN

LIB_EBIN = -pa _build/default/lib/*/ebin
export LIB_EBIN

TERMBOX_EBIN = -pa _checkouts/termbox2_nif/_build/default/lib/termbox2_nif/ebin
export TERMBOX_EBIN

rebar-shell:
	rebar3 shell --sname node1 --setcookie mysecretcookie 

demo:
	erl -sname node1 -setcookie mysecretcookie -noinput \
	$(LIB_EBIN) $(TERMBOX_EBIN) $(EXTRAS_EBIN) -eval 'demo:start()'

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

dual_editor:
	rebar3 compile 
	erl -sname node1 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'dual_editor:start()'

dual_editor_nofocus:
	rebar3 compile 
	erl -sname node1 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'dual_editor_nofocus:start()'

boxes:
	rebar3 compile 
	erl -sname node1 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'boxes:start()'

progress:
	rebar3 compile
	erl -sname node2 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'progress_demo:start()'
