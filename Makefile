DIALYZER = dialyzer
REBAR = ../cowboy-examples/rebar

all: app

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump
