## Makefile.am
##
## Process this file with automake to produce Makefile.in

ERLCFLAGS += -I ${ERLANG_LIB_DIR_map}/include

appdir = $(beamdir)
app_DATA = cgf.app
EXTRA_DIST = cgf.app

beam_DATA = cgf_app.beam \
		GenericChargingDataTypes.beam \
		IMSChargingDataTypes.beam
#		CSChargingDataTypes.beam \
#		GPRSChargingDataTypes.beam \
#		MMTelChargingDataTypes.beam \
#		SMSChargingDataTypes.beam \
#		MMSChargingDataTypes.beam \
#		CPDTChargingDataTypes.beam \
#		MBMSChargingDataTypes.beam \
#		POCChargingDataTypes.beam \
#		LCSChargingDataTypes.beam \
#		ProSeChargingDataTypes.beam \
#		MONTEChargingDataTypes.beam \
#		CHFChargingDataTypes.beam \
#		ExposureFunctionAPIChargingDataTypes.beam

%.beam:$(top_builddir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/src $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

