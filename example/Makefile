include ../include.mk

##############################################################################
# What to build

HRLS=
MODULES=ticker \
        printer \
        run_sample

##############################################################################
# Variables

BIN_FILES=$(MODULES:%=%.$(EMULATOR))
EBIN_FILES=$(BIN_FILES)
ERLC_FLAGS+=-W -I ../include
MODULES_COMMA=$(foreach a,$(MODULES),\'$(a)\',)
DEBUG_FLAGS=-Ddebug +debug_info


##############################################################################
# Targets

all: $(EBIN_FILES)

$(EBIN_FILES): $(HRLS)

clean:
	rm -f $(EBIN_FILES)

