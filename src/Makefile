##############################################################################
# Included make files

include ../include.mk
include ../vsn.mk
VERSION=$(OPENRTM_ERL_VERSION)


##############################################################################
# Subdirectories

SUBDIRS=idl


##############################################################################
# What to build

HRLS=
MODULES=openrtm_erl \
        utils \
        nvlist \
        mgr_sup \
        log_mgr \
        terminal_logger \
        file_logger \
        event_fmt \
        config_svr \
        config_svr_tests \
        config \
        config_tests \
        config_set \
        config_set_tests \
        defaults \
        mod_svr \
        mod_svr_tests \
        mgr \
        ecs_sup \
        ec_sup \
        ec \
        ec_part \
        periodic_ec \
        periodic_ec_tests \
        ports_sup \
        port_sup \
        port_mgr \
        portsvc \
        datain_portsvc \
        dataout_portsvc \
        connection \
        conn_sup \
        conns_sup \
        null_connection \
        push_new \
        put_listener \
        buffer_svr \
        queue_buffer \
        rtcs_sup \
        rtc_sup \
        rtc \
        rtc_cfg_svr \
        rtc_cfg_svr_tests \
        dataflow_rtc \
        dataflow_rtc_tests \
        rtc_initialiser \
        rtc_finaliser \
        rtc_exiter \
        rtc_action_caller \
        mock_rtc \
        mock_rtc_behv \
        mock_rtc_behv_ports \
        naming \
        corba_obj_mgr \
        RTM_Manager_impl \
        RTC_ExecutionContextService_impl \
        OpenRTM_DataFlowComponent_impl \
        SDOPackage_Configuration_impl \
        RTC_PortService_impl \
        OpenRTM_InPortCdr_impl


##############################################################################
# Variables

LOG_WITHOUT_LOGGER=-DLOG_WITHOUT_LOGGER
BIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR))
EBIN_FILES=$(BIN_FILES) ../ebin/$(APP_FILE) ../ebin/$(APPUP_FILE)
ERLC_FLAGS+=-W -pa ../ -I ../include $(LOG_WITHOUT_LOGGER)
MODULES_COMMA=$(foreach a,$(MODULES),\'$(a)\',)

ifeq ($(DEBUG), debug)
    DEBUG_FLAGS=-Ddebug +debug_info
else
    DEBUG_FLAGS=
endif

APP_NAME=openrtm_erl
APP_FILE=$(APP_NAME).app
APP_FILE_SRC=$(APP_FILE).src
APPUP_FILE=$(APP_FILE)up
APPUP_FILE_SRC=$(APP_FILE)up.src


##############################################################################
# Targets

all: $(EBIN_FILES)
	@set -e; \
		for d in $(SUBDIRS); do \
			if [ -f $$d/Makefile ]; then \
				(cd $$d && $(MAKE) $@) || exit 1; \
			fi; \
		done

$(EBIN_FILES): $(HRLS)

debug:
	$(MAKE) DEBUG=debug

clean:
	rm -f $(EBIN_FILES)
	@set -e; \
		for d in $(SUBDIRS); do \
			if [ -f $$d/Makefile ]; then \
				(cd $$d && $(MAKE) $@) || exit 1; \
			fi; \
		done

