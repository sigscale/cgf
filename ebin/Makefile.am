## Makefile.am
##
## Process this file with automake to produce Makefile.in

ERLCFLAGS += -I ${ERLANG_LIB_DIR_map}/include

appdir = $(beamdir)
app_DATA = cgf.app
EXTRA_DIST = cgf.app

beam_DATA = cgf.beam \
		cgf_app.beam \
		cgf_sup.beam \
		cgf_server.beam \
		cgf_sftpd.beam \
		cgf_event_sup.beam \
		cgf_event.beam \
		cgf_event_server.beam \
		cgf_import_sup.beam \
		cgf_import_fsm.beam \
		cgf_log_sup.beam \
		cgf_log_server_sup.beam \
		cgf_log_server.beam \
		cgf_log_job_sup.beam \
		cgf_log.beam \
		cgf_log_codec_ecs.beam \
		cgf_lib.beam \
		cgf_tools.beam \
		GenericChargingDataTypes.beam \
		IMSChargingDataTypes.beam \
		CSChargingDataTypes.beam \
		GPRSChargingDataTypes.beam \
		MMTelChargingDataTypes.beam \
		SMSChargingDataTypes.beam \
		MMSChargingDataTypes.beam \
		CPDTChargingDataTypes.beam \
		MBMSChargingDataTypes.beam \
		POCChargingDataTypes.beam \
		LCSChargingDataTypes.beam \
		ProSeChargingDataTypes.beam \
		MONTEChargingDataTypes.beam \
		CHFChargingDataTypes.beam \
		ExposureFunctionAPIChargingDataTypes.beam \
		TAP-0312.beam \
		Huawei-CS.beam \
		cgf_tap.beam \
		cgf_tap_fsm.beam \
		cgf_3gpp_file.beam \
		cgf_gprs.beam \
		cgf_gprs_fsm.beam \
		cgf_cs.beam \
		cgf_cs_fsm.beam \
		cgf_ims.beam \
		cgf_ims_fsm.beam \
		cgf_cs_huawei.beam \
		cgf_cs_huawei_fsm.beam

%.beam:$(top_builddir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

cgf_3gpp_file.beam:$(top_builddir)/src/cgf_3gpp_file.erl $(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/src $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

