## Makefile.am
##
## Process this file with automake to produce Makefile.in

ASNCFLAGS = $(ERLCFLAGS) \
		-I ${ERLANG_LIB_DIR_map}/asn_src \
		-I ${ERLANG_LIB_DIR_cap}/asn_src \
		+ber +noobj

noinst_DATA = GenericChargingDataTypes.erl \
		IMSChargingDataTypes.erl \
		CSChargingDataTypes.erl \
		GPRSChargingDataTypes.erl \
		MMTelChargingDataTypes.erl \
		SMSChargingDataTypes.erl \
		MMSChargingDataTypes.erl \
		CPDTChargingDataTypes.erl \
		MBMSChargingDataTypes.erl \
		POCChargingDataTypes.erl \
		LCSChargingDataTypes.erl \
		ProSeChargingDataTypes.erl \
		MONTEChargingDataTypes.erl \
		CHFChargingDataTypes.erl \
		ExposureFunctionAPIChargingDataTypes.erl \
		TAP-0312.erl \
		ssh_sftpd.hrl

einclude_DATA = GenericChargingDataTypes.hrl \
		IMSChargingDataTypes.hrl \
		CSChargingDataTypes.hrl \
		GPRSChargingDataTypes.hrl \
		MMTelChargingDataTypes.hrl \
		SMSChargingDataTypes.hrl \
		MMSChargingDataTypes.hrl \
		CPDTChargingDataTypes.hrl \
		MBMSChargingDataTypes.hrl \
		POCChargingDataTypes.hrl \
		LCSChargingDataTypes.hrl \
		ProSeChargingDataTypes.hrl \
		MONTEChargingDataTypes.hrl \
		CHFChargingDataTypes.hrl \
		ExposureFunctionAPIChargingDataTypes.hrl \
		TAP-0312.hrl

%.erl %.hrl:$(top_srcdir)/asn_src/%.asn1
	$(ERLC) $(ASNCFLAGS) $<

ssh_sftpd.hrl:
	sed -ne '/^-record(state,/,/})./p' \
			${ERLANG_LIB_DIR_ssh}/src/ssh_sftpd.erl > $@

CLEANFILES = $(noinst_DATA) \
		$(einclude_DATA) \
		*.asn1db

