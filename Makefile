.PHONY: all compile clean test shell run

export EXTRAS_EBIN = -pa ./_build/default/extras/examples/
export LIB_EBIN =    -pa _build/default/lib/*/ebin

all: compile

compile:
	@rebar3 compile

clean:
	@rebar3 clean

test:
	@rebar3 eunit

shell:
	@rebar3 shell

docs:
	@rebar3 ex_doc

# The `run` target will execute a given example. The example name can be passed as a parameter.
# For example: `make run example=counter`
run: compile
	@echo "Starting example: $(example)"
	@erl -noinput -sname cellium_dev -setcookie cellium_cookie  $(LIB_EBIN)  $(EXTRAS_EBIN) -eval "$(example):start()."

