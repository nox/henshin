DIALYZER = dialyzer
REBAR = rebar
PLT = .henshin_dialyzer.plt

.PHONY: all
all: compile

.PHONY: compile
compile:
	@$(REBAR) compile

.PHONY: deps
deps:
	@$(REBAR) get-deps

.PHONY: plt
plt: $(PLT)

$(PLT):
	@$(DIALYZER) --build_plt \
		-pa deps/*/ebin \
		--apps kernel stdlib syntax_tools \
		--output_plt $@

.PHONY: dialyze
dialyze: $(PLT) compile
	@$(DIALYZER) --plt $(PLT) ebin

.PHONY: test
test: ct

.PHONY: ct
ct:
	@$(REBAR) ct skip_deps=true
