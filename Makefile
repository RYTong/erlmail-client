compile:
	@./rebar compile

clean:
	@./rebar clean

deps:
	@./rebar get-deps

.PHONY: compile clean deps
