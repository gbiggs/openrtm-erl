SUBDIRS = src doc

include include.mk

all debug clean:
	@if [ ! -e ebin ]; then mkdir ebin; fi
	@set -e; \
		for d in $(SUBDIRS); do \
			if [ -f $$d/Makefile ]; then \
				(cd $$d && $(MAKE) $@) || exit 1; \
			fi; \
		done

doc:
	@erl -noshell -run edoc_run application "openrtm_erl" '"."' \
		'[{def,{vsn,"$(VSN)"}}, {subpackages, false}]'

test:
	@run_test -include $(PWD)/include/ -include $(PWD)/src/ -dir $(PWD) -logdir $(PWD)/test/logs/ -pa $(PWD)/ebin

eunit:
	@erl -noshell -run eunit test 'ebin'

types:
	@typer -I $(PWD)/include/ -I $(PWD)/src/ src/*.erl test/*.erl

.PHONY: doc test types

