MODULE=bowling
BEAMS=bowling.beam
ERLC_FLAGS=-DTEST

.PHONY: test
test : $(BEAMS)
	erl -noinput -run $(MODULE) verbose -s init stop

.PHONY: clean
clean :
	rm -f $(BEAMS)

%.beam : %.erl
	erlc $(ERLC_FLAGS) $^

bowling.plt :
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib sasl eunit

.PHONY: dialyzer
dialyzer : bowling.plt
	dialyzer --plt $^ bowling.erl
