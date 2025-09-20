.PHONY: test

test:
	 rebar3 shell --eval "demo:simple()."

all:
	rebar3 shell --eval "demo:start()."
