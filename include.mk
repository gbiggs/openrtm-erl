ERL=$(shell which erl)
ERLC=$(shell which erlc)
WELRC=$(shell which werlc)
EDOC=$(shell which edoc)
EMULATOR=beam
ERLC_FLAGS+="-Ddebug +trace +debug_info"
IDL_FLAGS="+{light_ifr,true}"

../ebin/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

../../ebin/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../../ebin $<

%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) $<

../ebin/%.app: %.app.src ../vsn.mk Makefile
	@sed -e "s;%VERSION%;$(VERSION);" \
		-e "s;%MODULES%;$(MODULES_COMMA);" \
		-e "s/,]/]/" \
		$< > $@

../ebin/%.appup: %.appup.src ../vsn.mk
	@sed -e 's;%VERSION%;$(VERSION);' \
		$< > $@

.idl:
	$(ERLC) $(IDL_FLAGS) $<

