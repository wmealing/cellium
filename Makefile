.PHONY: test go funky

EXTRAS_EBIN = -pa ./_build/default/extras/examples/
export EXTRAS_EBIN

LIB_EBIN = -pa _build/default/lib/*/ebin
export LIB_EBIN

TERMBOX_EBIN = -pa _checkouts/termbox2_nif/_build/default/lib/termbox2_nif/ebin
export TERMBOX_EBIN

widget_demo:
	erl -sname node1 -setcookie mysecretcookie -noshell  -noinput \
	$(LIB_EBIN) $(TERMBOX_EBIN) $(EXTRAS_EBIN) -eval 'widget_demo:start()'

table_demo:
	erl -sname node1 -setcookie mysecretcookie -noshell  -noinput \
	$(LIB_EBIN) $(TERMBOX_EBIN) $(EXTRAS_EBIN) -eval 'table_demo:start()'

advanced_table_demo:
	erl -sname node1 -setcookie mysecretcookie -noshell  -noinput \
	$(LIB_EBIN) $(TERMBOX_EBIN) $(EXTRAS_EBIN) -eval 'advanced_table_demo:start()'

node1:
	erl -sname node1 -setcookie mysecretcookie -noshell  -noinput $(LIB_EBIN) $(TERMBOX_EBIN) $(EXTRAS_EBIN)

rebar-shell:
	rebar3 shell --sname node1 --setcookie mysecretcookie 

demo:
	erl -sname node1 -setcookie mysecretcookie -noinput \
	$(LIB_EBIN) $(TERMBOX_EBIN) $(EXTRAS_EBIN) -eval 'demo:start()'

remsh:
	erl -sname node2 -setcookie mysecretcookie -remsh node1

test:
	rebar3 shell --eval "demo:simple()."

editor:
	erl -sname node1 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin $(EXTRAS_EBIN) -eval 'editor:start()'

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

mouseloc:
	rebar3 compile
	erl -sname node2 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'mouse_location:start()'


fault/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'progress_demo:start()'

floating:
	rebar3 compile
	erl -sname node2 -setcookie mysecretcookie -noshell  -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'floating:start()'

checkbox_demo:
	rebar3 compile
	erl -sname node1 -setcookie mysecretcookie -noshell -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'checkbox_demo:start()'

editable_table_demo:
	rebar3 compile
	erl -sname node1 -setcookie mysecretcookie -noshell -pa ./_build/default/checkouts/termbox2_nif/ebin -pa ./_build/default/lib/*/ebin -pa ./_build/default/extras/examples/ -eval 'editable_table_demo:start()'



