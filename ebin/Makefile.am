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
		cgf_statistics_sup.beam \
		cgf_statistics_server.beam \
		cgf_event_sup.beam \
		cgf_event.beam \
		cgf_event_server.beam \
		cgf_import_sup.beam \
		cgf_import_fsm.beam \
		cgf_sftpd.beam \
		cgf_log_sup.beam \
		cgf_log_server_sup.beam \
		cgf_log_server.beam \
		cgf_log_job_sup.beam \
		cgf_log.beam \
		cgf_log_codec_ecs.beam \
		cgf_lib.beam \
		cgf_tools.beam \
		cgf_rest.beam \
		cgf_rest_res_health.beam \
		mod_cgf_rest_accepted_content.beam \
		mod_cgf_rest_get.beam \
		mod_cgf_rest_head.beam \
		user_default.beam \
		ACSE-1.beam \
		Attribute-ASN1Module.beam \
		CMIP-1.beam \
		InformationFramework.beam \
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

%.beam:$(top_srcdir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) $<

ACSE-1.beam:$(top_builddir)/src/ACSE-1.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

Attribute-ASN1Module.beam:$(top_builddir)/src/Attribute-ASN1Module.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

CMIP-1.beam:$(top_builddir)/src/CMIP-1.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

InformationFramework.beam:$(top_builddir)/src/InformationFramework.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

GenericChargingDataTypes.beam:$(top_builddir)/src/GenericChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

IMSChargingDataTypes.beam:$(top_builddir)/src/IMSChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

CSChargingDataTypes.beam:$(top_builddir)/src/CSChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

GPRSChargingDataTypes.beam:$(top_builddir)/src/GPRSChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

MMTelChargingDataTypes.beam:$(top_builddir)/src/MMTelChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

SMSChargingDataTypes.beam:$(top_builddir)/src/SMSChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

MMSChargingDataTypes.beam:$(top_builddir)/src/MMSChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

CPDTChargingDataTypes.beam:$(top_builddir)/src/CPDTChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

MBMSChargingDataTypes.beam:$(top_builddir)/src/MBMSChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

POCChargingDataTypes.beam:$(top_builddir)/src/POCChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

LCSChargingDataTypes.beam:$(top_builddir)/src/LCSChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

ProSeChargingDataTypes.beam:$(top_builddir)/src/ProSeChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

MONTEChargingDataTypes.beam:$(top_builddir)/src/MONTEChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

CHFChargingDataTypes.beam:$(top_builddir)/src/CHFChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

ExposureFunctionAPIChargingDataTypes.beam:$(top_builddir)/src/ExposureFunctionAPIChargingDataTypes.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

TAP-0312.beam:$(top_builddir)/src/TAP-0312.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

Huawei-CS.beam:$(top_builddir)/src/Huawei-CS.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

cgf_sftpd.beam:$(top_srcdir)/src/cgf_sftpd.erl	$(top_builddir)/src/ssh_sftpd.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

cgf_3gpp_file.beam:$(top_srcdir)/src/cgf_3gpp_file.erl $(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

cgf_tap_fsm.beam:$(top_srcdir)/src/cgf_tap_fsm.erl	$(top_builddir)/ebin/cgf_import_fsm.beam
	$(ERLC) $(ERLCFLAGS) -pa $(top_builddir)/ebin $<

cgf_gprs.beam:$(top_srcdir)/src/cgf_gprs.erl	$(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

cgf_gprs_fsm.beam:$(top_srcdir)/src/cgf_gprs_fsm.erl	$(top_builddir)/ebin/cgf_import_fsm.beam $(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -pa $(top_builddir)/ebin -I $(top_srcdir)/include $<

cgf_ims.beam:$(top_srcdir)/src/cgf_ims.erl	$(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

cgf_ims_fsm.beam:$(top_srcdir)/src/cgf_ims_fsm.erl	$(top_builddir)/ebin/cgf_import_fsm.beam $(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -pa $(top_builddir)/ebin -I $(top_srcdir)/include $<

cgf_cs.beam:$(top_srcdir)/src/cgf_cs.erl	$(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

cgf_cs_fsm.beam:$(top_srcdir)/src/cgf_cs_fsm.erl	$(top_builddir)/ebin/cgf_import_fsm.beam $(top_srcdir)/include/cgf_3gpp_file.hrl
	$(ERLC) $(ERLCFLAGS) -pa $(top_builddir)/ebin -I $(top_srcdir)/include $<

cgf_cs_huawei_fsm.beam:$(top_srcdir)/src/cgf_cs_huawei_fsm.erl	$(top_builddir)/ebin/cgf_import_fsm.beam
	$(ERLC) $(ERLCFLAGS) -pa $(top_builddir)/ebin $<

%.app:	%.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

