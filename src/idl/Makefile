##############################################################################
# Included make files

include ../../include.mk


##############################################################################
# What to build

IDL=BasicDataType \
    DataPort \
    ExtendedDataTypes \
    InterfaceDataTypes \
    Manager \
    OpenRTM \
    RTC \
    SDOPackage

include idl_modules.mk
include idl_hrl.mk


##############################################################################
# Variables

ERL_FILES=$(MODULES:%=%.erl)
BIN_FILES=$(MODULES:%=../../ebin/%.$(EMULATOR))
EBIN_FILES=$(BIN_FILES)
ERLC_FLAGS+=-W -pa ../../ -I ../../include

ifeq ($(DEBUG), debug)
    DEBUG_FLAGS=-Ddebug +debug_info
else
    DEBUG_FLAGS=
endif


##############################################################################
# Targets

all: $(EBIN_FILES)

$(EBIN_FILES): $(HRLS)

debug:
	$(MAKE) DEBUG=debug

clean:
	@rm -f $(ERL_FILES) $(HRL)
	@rm -f $(EBIN_FILES)

