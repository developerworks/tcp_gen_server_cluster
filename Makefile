.PHONY: rel stagedevrel deps test

all: deps compile

compile: deps
	rebar compile

deps:
	test -d deps || rebar get-deps

clean:
	rebar clean

distclean: clean
	rebar delete-deps

test:
	rebar compile eunit
docs:
	rebar doc

gen:
	rebar generate