%%% cgf_gprs.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 - 2025 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements 3GPP GPRS CDR file handling in
%%% 	the {@link //cgf. cgf} application.
%%%
%%% @reference 3GPP TS <a
%%% 	href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=32&amp;GSMSpecPart2=298"
%%% 	>32.298</a> Charging Data Record (CDR) Parameter Description.
%%%
-module(cgf_gprs).
-copyright('Copyright (c) 2024 - 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% export the public API
-export([import/2, import/3]).

%% export the private API
-export([parse/3]).

-include_lib("kernel/include/logger.hrl").

%%----------------------------------------------------------------------
%%  The cgf_gprs public API
%%----------------------------------------------------------------------

-spec import(File, Log) -> Result
	when
		File :: file:filename() | binary(),
		Log :: disk_log:log(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv import(File, Log, [])
import(File, Log) ->
	import(File, Log, []).

-spec import(File, Log, Metadata) -> Result
	when
		File :: file:filename() | binary(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Import CDR file and write to Bx interface log.
import(File, Log, Metadata) when is_list(File) ->
	case file:read_file(File) of
		{ok, Bin} ->
			import1(Log, Metadata, Bin);
		{error, Reason} ->
			{error, Reason}
	end;
import(Bin, Log, Metadata)
		when is_binary(Bin), is_list(Metadata) ->
	import1(Log, Metadata, Bin).

%% @hidden
import1(Log, Metadata, Bin) ->
	import2(Log, Metadata,
			'GPRSChargingDataTypes':decode('GPRSRecord', Bin)).

%% @hidden
import2(Log, Metadata, {ok, CDR, Rest}) ->
	case parse(Log, Metadata, CDR) of
		ok when byte_size(Rest) == 0 ->
			ok;
		ok ->
			import1(Log, Metadata, Rest);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, element(1, CDR)}, {error, Reason}]),
			import1(Log, Metadata, Rest)
	end;
import2(_Log, _Metadata, {error, Reason}) ->
	{error, Reason}.

%%----------------------------------------------------------------------
%%  The cgf_gprs private API
%%----------------------------------------------------------------------

-spec parse(Log, Metadata, CDR) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDR :: {RecordType, Record},
		RecordType :: sgsnPDPRecord | ggsnPDPRecord
				| sgsnMMRecord | sgsnSMORecord | sgsnSMTRecord
				| sgsnMTLCSRecord | sgsnMOLCSRecord
				| sgsnNILCSRecord | sgsnMBMSRecord | ggsnMBMSRecord
				| sGWRecord | pGWRecord | gwMBMSRecord | tDFRecord
				| iPERecord | ePDGRecord | tWAGRecord,
		Record :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR.
%% @private
parse(Log, Metadata, {sgsnPDPRecord, SGSNPDPRecord} = _CDR) ->
	parse_sgsn_pdp(Log, Metadata, SGSNPDPRecord);
parse(Log, Metadata, {ggsnPDPRecord, GGSNPDPRecord}) ->
	parse_ggsn_pdp(Log, Metadata, GGSNPDPRecord);
parse(Log, Metadata, {sgsnMMRecord, SGSNMMRecord}) ->
	parse_sgsn_mmr(Log, Metadata, SGSNMMRecord);
parse(Log, Metadata, {sgsnSMORecord, SGSNSMORecord}) ->
	parse_sgsn_smo(Log, Metadata, SGSNSMORecord);
parse(Log, Metadata, {sgsnSMTRecord, SGSNSMTRecord}) ->
	parse_sgsn_smt(Log, Metadata, SGSNSMTRecord);
parse(Log, Metadata, {sgsnMTLCSRecord, SGSNMTLCSRecord}) ->
	parse_sgsn_mt_lcs(Log, Metadata, SGSNMTLCSRecord);
parse(Log, Metadata, {sgsnMOLCSRecord, SGSNMOLCSRecord}) ->
	parse_sgsn_mo_lcs(Log, Metadata, SGSNMOLCSRecord);
parse(Log, Metadata, {sgsnNILCSRecord, SGSNNILCSRecord}) ->
	parse_sgsn_ni_lcs(Log, Metadata, SGSNNILCSRecord);
parse(Log, Metadata, {sgsnMBMSRecord, SGSNMBMSRecord}) ->
	parse_sgsn_mbms(Log, Metadata, SGSNMBMSRecord);
parse(Log, Metadata, {ggsnMBMSRecord, GGSNMBMSRecord}) ->
	parse_ggsn_mbms(Log, Metadata, GGSNMBMSRecord);
parse(Log, Metadata, {sGWRecord, SGWRecord}) ->
	parse_sgw(Log, Metadata, SGWRecord);
parse(Log, Metadata, {pGWRecord, PGWRecord}) ->
	parse_pgw(Log, Metadata, PGWRecord);
parse(Log, Metadata, {gwMBMSRecord, GWMBMSRecord}) ->
	parse_gw_mbms(Log, Metadata, GWMBMSRecord);
parse(Log, Metadata, {tDFRecord, TDFRecord}) ->
	parse_tdf(Log, Metadata, TDFRecord);
parse(Log, Metadata, {iPERecord, IPERecord}) ->
	parse_ipe(Log, Metadata, IPERecord);
parse(Log, Metadata, {ePDGRecord, EPDGRecord}) ->
	parse_epdg(Log, Metadata, EPDGRecord);
parse(Log, Metadata, {tWAGRecord, TWAGRecord}) ->
	parse_twag(Log, Metadata, TWAGRecord).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse_sgsn_pdp(Log, Metadata, SGSNPDPRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNPDPRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN PDP.
parse_sgsn_pdp(Log, Metadata, SGSNPDPRecord) ->
	Call = sgsn_pdp_record(SGSNPDPRecord),
	CDR = [{sgsnPDP, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_ggsn_pdp(Log, Metadata, GGSNPDPRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		GGSNPDPRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a GGSN PDP.
parse_ggsn_pdp(Log, Metadata, GGSNPDPRecord) ->
	Call = ggsn_pdp_record(GGSNPDPRecord),
	CDR = [{ggsnPDP, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_sgsn_mmr(Log, Metadata, SGSNMMRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNMMRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN MMR.
parse_sgsn_mmr(Log, Metadata, SGSNMMRecord) ->
	Call = sgsn_mmr(SGSNMMRecord),
	CDR = [{sgsnMM, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_sgsn_smo(Log, Metadata, SGSNSMORecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNSMORecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN SMO.
parse_sgsn_smo(Log, Metadata, SGSNSMORecord) ->
	SMS = sgsn_smo(SGSNSMORecord),
	CDR = [{sgsnSMO, SMS} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_sgsn_smt(Log, Metadata, SGSNSMTRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNSMTRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN SMT.
parse_sgsn_smt(Log, Metadata, SGSNSMTRecord) ->
	SMS = sgsn_smt(SGSNSMTRecord),
	CDR = [{sgsnSMT, SMS} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_sgsn_mt_lcs(Log, Metadata, SGSNMTLCSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNMTLCSRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN MT LCS.
parse_sgsn_mt_lcs(_Log, _Metadata, _SGSNMTLCSRecord) ->
	{error, not_implemented}.

-spec parse_sgsn_mo_lcs(Log, Metadata, SGSNMOLCSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNMOLCSRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN MO LCS.
parse_sgsn_mo_lcs(_Log, _Metadata, _SGSNMOLCSRecord) ->
	{error, not_implemented}.

-spec parse_sgsn_ni_lcs(Log, Metadata, SGSNNILCSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNNILCSRecord:: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN NI LCS.
parse_sgsn_ni_lcs(_Log, _Metadata, _SGSNNILCSRecord) ->
	{error, not_implemented}.

-spec parse_sgsn_mbms(Log, Metadata, SGSNMBMSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGSNMBMSRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGSN MBMS.
parse_sgsn_mbms(_Log, _Metadata, _SGSNMBMSRecord) ->
	{error, not_implemented}.

-spec parse_ggsn_mbms(Log, Metadata, GGSNMBMSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		GGSNMBMSRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a GGSN MBMS.
parse_ggsn_mbms(_Log, _Metadata, _GGSNMBMSRecord) ->
	{error, not_implemented}.

-spec parse_sgw(Log, Metadata, SGWRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SGWRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SGW.
parse_sgw(_Log, _Metadata, _SGWRecord) ->
	{error, not_implemented}.

-spec parse_pgw(Log, Metadata, PGWRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		PGWRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a PGW.
parse_pgw(Log, Metadata, PGWRecord) ->
	Call = pgw_record(PGWRecord),
	CDR = [{pgw_record, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_gw_mbms(Log, Metadata, GWMBMSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		GWMBMSRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a GW MBMS.
parse_gw_mbms(_Log, _Metadata, _GWMBMSRecord) ->
	{error, not_implemented}.

-spec parse_tdf(Log, Metadata, TDFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		TDFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a TDF.
parse_tdf(_Log, _Metadata, _TDFRecord) ->
	{error, not_implemented}.

-spec parse_ipe(Log, Metadata, IPERecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		IPERecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an IPE.
parse_ipe(_Log, _Metadata, _IPERecord) ->
	{error, not_implemented}.

-spec parse_epdg(Log, Metadata, EPDGRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		EPDGRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an EPDG.
parse_epdg(_Log, _Metadata, _EPDGRecord) ->
	{error, not_implemented}.

-spec parse_twag(Log, Metadata, TWAGRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		TWAGRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a TWAG.
parse_twag(_Log, _Metadata, _TWAGRecord) ->
	{error, not_implemented}.

%% @hidden
sgsn_pdp_record(#{accessPointNameNI
		:= APNNI} = SGSNPDPRecord) ->
	Acc = #{<<"accessPointNameNI">> => APNNI},
	sgsn_pdp_record1(SGSNPDPRecord, Acc);
sgsn_pdp_record(SGSNPDPRecord) ->
	sgsn_pdp_record1(SGSNPDPRecord, #{}).
%% @hidden
sgsn_pdp_record1(#{accessPointNameOI
		:= APNOI} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"accessPointNameOI">> => APNOI},
	sgsn_pdp_record2(SGSNPDPRecord, Acc1);
sgsn_pdp_record1(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record2(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record2(#{apnSelectionMode
		:= APNSM} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"apnSelectionMode">> => APNSM},
	sgsn_pdp_record3(SGSNPDPRecord, Acc1);
sgsn_pdp_record2(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record3(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record3(#{causeForRecClosing
		:= CFRC} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"causeForRecClosing">> => CFRC},
	sgsn_pdp_record4(SGSNPDPRecord, Acc1);
sgsn_pdp_record3(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record4(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record4(#{cellIdentifier
		:= CI} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"cellIdentifier">> => cgf_lib:octet_string(CI)},
	sgsn_pdp_record5(SGSNPDPRecord, Acc1);
sgsn_pdp_record4(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record5(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record5(#{chChSelectionMode
		:= CHCHSM} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"chChSelectionMode">> => CHCHSM},
	sgsn_pdp_record6(SGSNPDPRecord, Acc1);
sgsn_pdp_record5(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record6(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record6(#{chargingCharacteristics
		:= CC} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"chargingCharacteristics">> => cgf_lib:octet_string(CC)},
	sgsn_pdp_record7(SGSNPDPRecord, Acc1);
sgsn_pdp_record6(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record7(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record7(#{chargingID
		:= CID} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"chargingID">> => CID},
	sgsn_pdp_record8(SGSNPDPRecord, Acc1);
sgsn_pdp_record7(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record8(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record8(#{diagnostics := Diagnostics} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	sgsn_pdp_record9(SGSNPDPRecord, Acc1);
sgsn_pdp_record8(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record9(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record9(#{duration
		:= Duration} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"duration">> => Duration},
	sgsn_pdp_record10(SGSNPDPRecord, Acc1);
sgsn_pdp_record9(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record10(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record10(#{dynamicAddressFlag
		:= DAF} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"dynamicAddressFlag">> => DAF},
	sgsn_pdp_record11(SGSNPDPRecord, Acc1);
sgsn_pdp_record10(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record11(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record11(#{ggsnAddressUsed := Address} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"ggsnAddressUsed">> => cgf_lib:ip_address(Address)},
	sgsn_pdp_record12(SGSNPDPRecord, Acc1);
sgsn_pdp_record11(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record12(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record12(#{listOfTrafficVolumes
		:= ListOfTrafficVolumes} = SGSNPDPRecord, Acc) ->
	LOTV = [traffic_volumes(TV) || TV <- ListOfTrafficVolumes],
	Acc1 = Acc#{<<"listOfTrafficVolumes">> => LOTV},
	sgsn_pdp_record13(SGSNPDPRecord, Acc1);
sgsn_pdp_record12(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record13(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record13(#{localSequenceNumber
		:= LocalSequenceNumber} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"localSequenceNumber">> => LocalSequenceNumber},
	sgsn_pdp_record14(SGSNPDPRecord, Acc1);
sgsn_pdp_record13(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record14(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record14(#{locationAreaCode
		:= LocationAreaCode} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"locationAreaCode">> => cgf_lib:octet_string(LocationAreaCode)},
	sgsn_pdp_record15(SGSNPDPRecord, Acc1);
sgsn_pdp_record14(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record15(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record15(#{msNetworkCapability
		:= MSNetworkCapability} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"msNetworkCapability">> =>
			cgf_lib:octet_string(MSNetworkCapability)},
	sgsn_pdp_record16(SGSNPDPRecord, Acc1);
sgsn_pdp_record15(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record16(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record16(#{lowPriorityIndicator
		:= _LowPriorityIndicator} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"lowPriorityIndicator">> => undefined},
	sgsn_pdp_record17(SGSNPDPRecord, Acc1);
sgsn_pdp_record16(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record17(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record17(#{networkInitiation
		:= NetworkInitiation} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"networkInitiation">> => NetworkInitiation},
	sgsn_pdp_record18(SGSNPDPRecord, Acc1);
sgsn_pdp_record17(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record18(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record18(#{pdpType
		:= PDPType} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"pdpType">> => cgf_lib:octet_string(PDPType)},
	sgsn_pdp_record19(SGSNPDPRecord, Acc1);
sgsn_pdp_record18(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record19(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record19(#{rATType
		:= RATType} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"rATType">> => RATType},
	sgsn_pdp_record20(SGSNPDPRecord, Acc1);
sgsn_pdp_record19(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record20(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record20(#{rNCUnsentDownlinkVolume
		:= DownLinkVolume} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"rNCUnsentDownlinkVolume">> => DownLinkVolume},
	sgsn_pdp_record21(SGSNPDPRecord, Acc1);
sgsn_pdp_record20(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record21(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record21(#{recordExtensions
		:= RecordExtensions} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => RecordExtensions},
	sgsn_pdp_record22(SGSNPDPRecord, Acc1);
sgsn_pdp_record21(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record22(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record22(#{recordOpeningTime
		:= RecordOpeningTime} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(RecordOpeningTime)},
	sgsn_pdp_record23(SGSNPDPRecord, Acc1);
sgsn_pdp_record22(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record23(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record23(#{recordType := RecordType} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"recordType">> => RecordType},
	sgsn_pdp_record24(SGSNPDPRecord, Acc1);
sgsn_pdp_record23(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record24(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record24(#{routingArea := RoutingArea} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"routingArea">> => cgf_lib:octet_string(RoutingArea)},
	sgsn_pdp_record25(SGSNPDPRecord, Acc1);
sgsn_pdp_record24(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record25(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record25(#{servedIMEI := IMEI} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(IMEI)},
	sgsn_pdp_record26(SGSNPDPRecord, Acc1);
sgsn_pdp_record25(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record26(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record26(#{servedIMSI := IMSI} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMSI">> => cgf_lib:tbcd(IMSI)},
	sgsn_pdp_record27(SGSNPDPRecord, Acc1);
sgsn_pdp_record26(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record27(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record27(#{servedMSISDN := ServedMSISDN} = SGSNPDPRecord, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	sgsn_pdp_record28(SGSNPDPRecord, Acc1);
sgsn_pdp_record27(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record28(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record28(#{servedPDPAddress
		:= {iPAddress, Address}} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedPDPAddress">> => cgf_lib:ip_address(Address)},
	sgsn_pdp_record29(SGSNPDPRecord, Acc1);
sgsn_pdp_record28(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record29(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record29(#{servedPDPPDNAddressExt
		:= {iPAddress, Address}} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedPDPPDNAddressExt">> => cgf_lib:ip_address(Address)},
	sgsn_pdp_record30(SGSNPDPRecord, Acc1);
sgsn_pdp_record29(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record30(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record30(#{servingNodePLMNIdentifier := Identifer}
		= SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servingNodePLMNIdentifier">> => cgf_lib:octet_string(Identifer)},
	sgsn_pdp_record31(SGSNPDPRecord, Acc1);
sgsn_pdp_record30(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record31(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record31(#{sgsnAddress := Address} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"sgsnAddress">> => cgf_lib:ip_address(Address)},
	sgsn_pdp_record32(SGSNPDPRecord, Acc1);
sgsn_pdp_record31(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record32(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record32(#{sgsnChange := SgsnChange}
		= SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"sgsnChange">> => SgsnChange},
	sgsn_pdp_record33(SGSNPDPRecord, Acc1);
sgsn_pdp_record32(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record33(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record33(#{userCSGInformation := UserCSGInformation}
		= _SGSNPDPRecord, Acc) ->
	Acc#{<<"userCSGInformation">> => UserCSGInformation};
sgsn_pdp_record33(_SGSNPDPRecord, Acc) ->
		Acc.

%% @hidden
ggsn_pdp_record(#{networkInitiation := NetworkInitiation} = GGSNPDPRecord) ->
	Acc = #{<<"networkInitiation">> => NetworkInitiation},
	ggsn_pdp_record1(GGSNPDPRecord, Acc);
ggsn_pdp_record(GGSNPDPRecord) ->
	ggsn_pdp_record1(GGSNPDPRecord, #{}).
%% @hidden
ggsn_pdp_record1(#{servedIMSI := IMSI} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMSI">> => cgf_lib:tbcd(IMSI)},
	ggsn_pdp_record2(GGSNPDPRecord, Acc1);
ggsn_pdp_record1(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record2(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record2(#{ggsnAddress := Address} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"ggsnAddress">> => cgf_lib:ip_address(Address)},
	ggsn_pdp_record3(GGSNPDPRecord, Acc1);
ggsn_pdp_record2(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record3(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record3(#{chargingID := ID} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"chargingID">> => ID},
	ggsn_pdp_record4(GGSNPDPRecord, Acc1);
ggsn_pdp_record3(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record4(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record4(#{sgsnAddress := SA} = GGSNPDPRecord, Acc) ->
	Address = [cgf_lib:ip_address(A) || A <- SA],
	Acc1 = Acc#{<<"sgsnAddress">> => Address},
	ggsn_pdp_record5(GGSNPDPRecord, Acc1);
ggsn_pdp_record4(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record5(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record5(#{accessPointNameNI := APNNI} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"accessPointNameNI">> => APNNI},
	ggsn_pdp_record6(GGSNPDPRecord, Acc1);
ggsn_pdp_record5(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record6(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record6(#{pdpType := PDPType} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"pdpType">> => cgf_lib:octet_string(PDPType)},
	ggsn_pdp_record7(GGSNPDPRecord, Acc1);
ggsn_pdp_record6(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record7(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record7(#{servedPDPAddress
		:= {iPAddress, Address}} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedPDPAddress">> => cgf_lib:ip_address(Address)},
	ggsn_pdp_record8(GGSNPDPRecord, Acc1);
ggsn_pdp_record7(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record8(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record8(#{dynamicAddressFlag := DAF} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"dynamicAddressFlag">> => DAF},
	ggsn_pdp_record9(GGSNPDPRecord, Acc1);
ggsn_pdp_record8(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record9(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record9(#{listOfTrafficVolumes
		:= ListOfTrafficVolumes} = GGSNPDPRecord, Acc) ->
	LOTV = [traffic_volumes(TV) || TV <- ListOfTrafficVolumes],
	Acc1 = Acc#{<<"listOfTrafficVolumes">> => LOTV},
	ggsn_pdp_record10(GGSNPDPRecord, Acc1);
ggsn_pdp_record9(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record10(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record10(#{recordOpeningTime
		:= RecordOpeningTime} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(RecordOpeningTime)},
	ggsn_pdp_record11(GGSNPDPRecord, Acc1);
ggsn_pdp_record10(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record11(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record11(#{duration := Duration} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"duration">> => Duration},
	ggsn_pdp_record12(GGSNPDPRecord, Acc1);
ggsn_pdp_record11(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record12(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record12(#{causeForRecClosing := CFRC} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"causeForRecClosing">> => CFRC},
	ggsn_pdp_record13(GGSNPDPRecord, Acc1);
ggsn_pdp_record12(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record13(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record13(#{diagnostics := Diagnostics} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	ggsn_pdp_record14(GGSNPDPRecord, Acc1);
ggsn_pdp_record13(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record14(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record14(#{recordSequenceNumber
		:= SequenceNumber} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"recordSequenceNumber">> => SequenceNumber},
	ggsn_pdp_record15(GGSNPDPRecord, Acc1);
ggsn_pdp_record14(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record15(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record15(#{nodeID := Identifier} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"nodeID">> => Identifier},
	ggsn_pdp_record16(GGSNPDPRecord, Acc1);
ggsn_pdp_record15(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record16(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record16(#{localSequenceNumber
		:= SequenceNumber} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"localSequenceNumber">> => SequenceNumber},
	ggsn_pdp_record17(GGSNPDPRecord, Acc1);
ggsn_pdp_record16(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record17(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record17(#{apnSelectionMode := APNSM} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"apnSelectionMode">> => APNSM},
	ggsn_pdp_record18(GGSNPDPRecord, Acc1);
ggsn_pdp_record17(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record18(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record18(#{servedMSISDN := ServedMSISDN} = GGSNPDPRecord, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	ggsn_pdp_record19(GGSNPDPRecord, Acc1);
ggsn_pdp_record18(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record19(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record19(#{chargingCharacteristics := CC} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"chargingCharacteristics">> => cgf_lib:octet_string(CC)},
	ggsn_pdp_record20(GGSNPDPRecord, Acc1);
ggsn_pdp_record19(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record20(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record20(#{chChSelectionMode := CHCHSM} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"chChSelectionMode">> => CHCHSM},
	ggsn_pdp_record21(GGSNPDPRecord, Acc1);
ggsn_pdp_record20(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record21(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record21(#{iMSsignalingContext := _} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"iMSsignalingContext">> => undefined},
	ggsn_pdp_record22(GGSNPDPRecord, Acc1);
ggsn_pdp_record21(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record22(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record22(#{externalChargingID := ID} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"externalChargingID">> => cgf_lib:octet_string(ID)},
	ggsn_pdp_record23(GGSNPDPRecord, Acc1);
ggsn_pdp_record22(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record23(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record23(#{sgsnPLMNIdentifier := ID} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"sgsnPLMNIdentifier">> => cgf_lib:octet_string(ID)},
	ggsn_pdp_record24(GGSNPDPRecord, Acc1);
ggsn_pdp_record23(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record24(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record24(#{servedIMEI := IMEI} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(IMEI)},
	ggsn_pdp_record25(GGSNPDPRecord, Acc1);
ggsn_pdp_record24(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record25(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record25(#{rATType := RATType} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"rATType">> => RATType},
	ggsn_pdp_record26(GGSNPDPRecord, Acc1);
ggsn_pdp_record25(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record26(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record26(#{mSTimeZone := TZ} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"mSTimeZone">> => cgf_lib:octet_string(TZ)},
	ggsn_pdp_record27(GGSNPDPRecord, Acc1);
ggsn_pdp_record26(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record27(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record27(#{userLocationInformation := LI} = GGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"userLocationInformation">> => cgf_lib:octet_string(LI)},
	ggsn_pdp_record28(GGSNPDPRecord, Acc1);
ggsn_pdp_record27(GGSNPDPRecord, Acc) ->
	ggsn_pdp_record28(GGSNPDPRecord, Acc).
%% @hidden
ggsn_pdp_record28(#{cAMELChargingInformation := CI}, Acc) ->
	Acc#{<<"cAMELChargingInformation">> => cgf_lib:octet_string(CI)};
ggsn_pdp_record28(_GGSNPDPRecord, Acc) ->
	Acc.

%% @hidden
traffic_volumes(#{accessAvailabilityChangeReason
		:= AAC} = TV) ->
	Acc = #{<<"accessAvailabilityChangeReason">> => AAC},
	traffic_volumes1(TV, Acc);
traffic_volumes(TV) ->
	traffic_volumes1(TV, #{}).
%% @hidden
traffic_volumes1(#{cPCIoTEPSOptimisationIndicator
		:= Indicator} = TV, Acc) ->
	Acc1 = Acc#{<<"cPCIoTEPSOptimisationIndicator">> => Indicator},
	traffic_volumes2(TV, Acc1);
traffic_volumes1(TV, Acc) ->
	traffic_volumes2(TV, Acc).
%% @hidden
traffic_volumes2(#{changeCondition := CC} = TV, Acc) ->
	Acc1 = Acc#{<<"changeCondition">> => CC},
	traffic_volumes3(TV, Acc1);
traffic_volumes2(TV, Acc) ->
	traffic_volumes3(TV, Acc).
%% @hidden
traffic_volumes3(#{changeTime := CT} = TV, Acc) ->
	Acc1 = Acc#{<<"changeTime">> => cgf_lib:bcd_date_time(CT)},
	traffic_volumes4(TV, Acc1);
traffic_volumes3(TV, Acc) ->
	traffic_volumes4(TV, Acc).
%% @hidden
traffic_volumes4(#{chargingID
		:= CI} = TV, Acc) ->
	Acc1 = Acc#{<<"chargingID">> => CI},
	traffic_volumes5(TV, Acc1);
traffic_volumes4(TV, Acc) ->
	traffic_volumes5(TV, Acc).
%% @hidden
traffic_volumes5(#{dataVolumeGPRSDownlink
		:= DownLink} = TV, Acc) ->
	Acc1 = Acc#{<<"dataVolumeGPRSDownlink">> => DownLink},
	traffic_volumes6(TV, Acc1);
traffic_volumes5(TV, Acc) ->
	traffic_volumes6(TV, Acc).
%% @hidden
traffic_volumes6(#{dataVolumeGPRSUplink
		:= UpLink} = TV, Acc) ->
	Acc1 = Acc#{<<"dataVolumeGPRSUplink">> => UpLink},
	traffic_volumes7(TV, Acc1);
traffic_volumes6(TV, Acc) ->
	traffic_volumes7(TV, Acc).
%% @hidden
traffic_volumes7(#{diagnostics := Diagnostics} = TV, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	traffic_volumes8(TV, Acc1);
traffic_volumes7(TV, Acc) ->
	traffic_volumes8(TV, Acc).
%% @hidden
traffic_volumes8(#{ePCQoSInformation
		:= EPCQoSInformation} = TV, Acc) ->
	Acc1 = Acc#{<<"ePCQoSInformation">> => EPCQoSInformation},
	traffic_volumes9(TV, Acc1);
traffic_volumes8(TV, Acc) ->
	traffic_volumes9(TV, Acc).
%% @hidden
traffic_volumes9(#{enhancedDiagnostics := Diagnostics} = TV, Acc) ->
	Acc1 = Acc#{<<"enhancedDiagnostics">> => enhanced_dianostics(Diagnostics)},
	traffic_volumes10(TV, Acc1);
traffic_volumes9(TV, Acc) ->
	traffic_volumes10(TV, Acc).
%% @hidden
traffic_volumes10(#{listOfPresenceReportingAreaInformation
		:= ListOfPRA} = TV, Acc) ->
	Acc1 = Acc#{<<"listOfPresenceReportingAreaInformation">> => ListOfPRA},
	traffic_volumes11(TV, Acc1);
traffic_volumes10(TV, Acc) ->
	traffic_volumes11(TV, Acc).
%% @hidden
traffic_volumes11(#{qosNegotiated
		:= QOSNegotiated} = TV, Acc) ->
	Acc1 = Acc#{<<"qosNegotiated">> => cgf_lib:octet_string(QOSNegotiated)},
	traffic_volumes12(TV, Acc1);
traffic_volumes11(TV, Acc) ->
	traffic_volumes12(TV, Acc).
%% @hidden
traffic_volumes12(#{qosRequested
		:= QOSRequested} = TV, Acc) ->
	Acc1 = Acc#{<<"qosRequested">> => cgf_lib:octet_string(QOSRequested)},
	traffic_volumes13(TV, Acc1);
traffic_volumes12(TV, Acc) ->
	traffic_volumes13(TV, Acc).
%% @hidden
traffic_volumes13(#{relatedChangeOfCharCondition
		:= ChangeOfCharCon} = TV, Acc) ->
	Acc1 = Acc#{<<"relatedChangeOfCharCondition">> => ChangeOfCharCon},
	traffic_volumes14(TV, Acc1);
traffic_volumes13(TV, Acc) ->
	traffic_volumes14(TV, Acc).
%% @hidden
traffic_volumes14(#{servingPLMNRateControl
		:= RateControl} = TV, Acc) ->
	Acc1 = Acc#{<<"servingPLMNRateControl">> => RateControl},
	traffic_volumes15(TV, Acc1);
traffic_volumes14(TV, Acc) ->
	traffic_volumes15(TV, Acc).
%% @hidden
traffic_volumes15(#{threeGPPPSDataOffStatus
		:= DataOffStatus} = TV, Acc) ->
	Acc1 = Acc#{<<"threeGPPPSDataOffStatus">> => DataOffStatus},
	traffic_volumes16(TV, Acc1);
traffic_volumes15(TV, Acc) ->
	traffic_volumes16(TV, Acc).
%% @hidden
traffic_volumes16(#{uWANUserLocationInformation
		:= WanUserLocInfo} = TV, Acc) ->
	Acc1 = Acc#{<<"uWANUserLocationInformation">> => WanUserLocInfo},
	traffic_volumes17(TV, Acc1);
traffic_volumes16(TV, Acc) ->
	traffic_volumes17(TV, Acc).
%% @hidden
traffic_volumes17(#{userCSGInformation
		:= CSGInfo} = TV, Acc) ->
	Acc1 = Acc#{<<"userCSGInformation">> => CSGInfo},
	traffic_volumes18(TV, Acc1);
traffic_volumes17(TV, Acc) ->
	traffic_volumes18(TV, Acc).
%% @hidden
traffic_volumes18(#{userLocationInformation
		:= UserLocInfo} = _TV, Acc) ->
	Acc#{<<"userLocationInformation">> => UserLocInfo};
traffic_volumes18(_TV, Acc) ->
	Acc.

%% @hidden
pgw_record(#{aPNRateControl := APNRateControl} = PGWRecord) ->
	Acc = #{<<"aPNRateControl">> => APNRateControl},
	pgw_record1(PGWRecord, Acc);
pgw_record(PGWRecord) ->
	pgw_record1(PGWRecord, #{}).
%% @hidden
pgw_record1(#{accessPointNameNI := APNNI} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"accessPointNameNI">> => APNNI},
	pgw_record2(PGWRecord, Acc1);
pgw_record1(PGWRecord, Acc) ->
	pgw_record2(PGWRecord, Acc).
%% @hidden
pgw_record2(#{apnSelectionMode := APNSelMode} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"apnSelectionMode">> => APNSelMode},
	pgw_record3(PGWRecord, Acc1);
pgw_record2(PGWRecord, Acc) ->
	pgw_record3(PGWRecord, Acc).
%% @hidden
pgw_record3(#{cAMELChargingInformation := CAMELInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"cAMELChargingInformation">> => CAMELInfo},
	pgw_record4(PGWRecord, Acc1);
pgw_record3(PGWRecord, Acc) ->
	pgw_record4(PGWRecord, Acc).
%% @hidden
pgw_record4(#{cNOperatorSelectionEnt := CNOpSelEnt} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"cNOperatorSelectionEnt">> => CNOpSelEnt},
	pgw_record5(PGWRecord, Acc1);
pgw_record4(PGWRecord, Acc) ->
	pgw_record5(PGWRecord, Acc).
%% @hidden
pgw_record5(#{causeForRecClosing := Cause} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"causeForRecClosing">> => Cause},
	pgw_record6(PGWRecord, Acc1);
pgw_record5(PGWRecord, Acc) ->
	pgw_record6(PGWRecord, Acc).
%% @hidden
pgw_record6(#{chChSelectionMode := ChChSelMode} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"chChSelectionMode">> => ChChSelMode},
	pgw_record7(PGWRecord, Acc1);
pgw_record6(PGWRecord, Acc) ->
	pgw_record7(PGWRecord, Acc).
%% @hidden
pgw_record7(#{chargingCharacteristics := CharChar} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"chargingCharacteristics">> => cgf_lib:octet_string(CharChar)},
	pgw_record8(PGWRecord, Acc1);
pgw_record7(PGWRecord, Acc) ->
	pgw_record8(PGWRecord, Acc).
%% @hidden
pgw_record8(#{chargingID := ChargingID} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"chargingID">> => ChargingID},
	pgw_record9(PGWRecord, Acc1);
pgw_record8(PGWRecord, Acc) ->
	pgw_record9(PGWRecord, Acc).
%% @hidden
pgw_record9(#{chargingPerIPCANSessionIndicator := ChargingIndicator} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"chargingPerIPCANSessionIndicator">> => ChargingIndicator},
	pgw_record10(PGWRecord, Acc1);
pgw_record9(PGWRecord, Acc) ->
	pgw_record10(PGWRecord, Acc).
%% @hidden
pgw_record10(#{diagnostics := Diagnostics} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	pgw_record11(PGWRecord, Acc1);
pgw_record10(PGWRecord, Acc) ->
	pgw_record11(PGWRecord, Acc).
%% @hidden
pgw_record11(#{duration := Duration} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"duration">> => Duration},
	pgw_record12(PGWRecord, Acc1);
pgw_record11(PGWRecord, Acc) ->
	pgw_record12(PGWRecord, Acc).
%% @hidden
pgw_record12(#{dynamicAddressFlag := DynAddrFlag} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"dynamicAddressFlag">> => DynAddrFlag},
	pgw_record13(PGWRecord, Acc1);
pgw_record12(PGWRecord, Acc) ->
	pgw_record13(PGWRecord, Acc).
%% @hidden
pgw_record13(#{dynamicAddressFlagExt := DynAddrFlagExt} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"dynamicAddressFlagExt">> => DynAddrFlagExt},
	pgw_record14(PGWRecord, Acc1);
pgw_record13(PGWRecord, Acc) ->
	pgw_record14(PGWRecord, Acc).
%% @hidden
pgw_record14(#{ePCQoSInformation := EPCQoSInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"ePCQoSInformation">> => epcqos_info(EPCQoSInfo)},
	pgw_record15(PGWRecord, Acc1);
pgw_record14(PGWRecord, Acc) ->
	pgw_record15(PGWRecord, Acc).
%% @hidden
pgw_record15(#{enhancedDiagnostics := Diagnostics} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"enhancedDiagnostics">> => enhanced_dianostics(Diagnostics)},
	pgw_record16(PGWRecord, Acc1);
pgw_record15(PGWRecord, Acc) ->
	pgw_record16(PGWRecord, Acc).
%% @hidden
pgw_record16(#{mSTimeZone := MSTZ} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"mSTimeZone">> => cgf_lib:octet_string(MSTZ)},
	pgw_record17(PGWRecord, Acc1);
pgw_record16(PGWRecord, Acc) ->
	pgw_record17(PGWRecord, Acc).
%% @hidden
pgw_record17(#{lastUserLocationInformation := LastUserLocInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"lastUserLocationInformation">> => cgf_lib:octet_string(LastUserLocInfo)},
	pgw_record18(PGWRecord, Acc1);
pgw_record17(PGWRecord, Acc) ->
	pgw_record18(PGWRecord, Acc).
%% @hidden
pgw_record18(#{listOfRANSecondaryRATUsageReports := ListRANUsage} = PGWRecord, Acc) ->
	ParsedList = [ran_usage(RANUsage) || RANUsage <- ListRANUsage],
	Acc1 = Acc#{<<"listOfRANSecondaryRATUsageReports">> => ParsedList},
	pgw_record19(PGWRecord, Acc1);
pgw_record18(PGWRecord, Acc) ->
	pgw_record19(PGWRecord, Acc).
%% @hidden
pgw_record19(#{listOfServiceData := ListServiceData} = PGWRecord, Acc) ->
	ParsedList = [service_data(ServiceData) || ServiceData <- ListServiceData],
	Acc1 = Acc#{<<"listOfServiceData">> => ParsedList},
	pgw_record20(PGWRecord, Acc1);
pgw_record19(PGWRecord, Acc) ->
	pgw_record20(PGWRecord, Acc).
%% @hidden
pgw_record20(#{listOfTrafficVolumes := ListTrafficVol} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"listOfTrafficVolumes">> => traffic_volumes(ListTrafficVol)},
	pgw_record21(PGWRecord, Acc1);
pgw_record20(PGWRecord, Acc) ->
	pgw_record21(PGWRecord, Acc).
%% @hidden
pgw_record21(#{localSequenceNumber:= LocalSeqNum} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"localSequenceNumber">> => LocalSeqNum},
	pgw_record22(PGWRecord, Acc1);
pgw_record21(PGWRecord, Acc) ->
	pgw_record22(PGWRecord, Acc).
%% @hidden
pgw_record22(#{mOExceptionDataCounter := MOExceptionDataCounter} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"mOExceptionDataCounter">> => mo_exception(MOExceptionDataCounter)},
	pgw_record23(PGWRecord, Acc1);
pgw_record22(PGWRecord, Acc) ->
	pgw_record23(PGWRecord, Acc).
%% @hidden
pgw_record23(#{lastMSTimeZone := MSTZ} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"lastMSTimeZone">> => cgf_lib:octet_string(MSTZ)},
	pgw_record24(PGWRecord, Acc1);
pgw_record23(PGWRecord, Acc) ->
	pgw_record24(PGWRecord, Acc).
%% @hidden
pgw_record24(#{nBIFOMMode := NBIFOMMode} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"nBIFOMMode">> => NBIFOMMode},
	pgw_record25(PGWRecord, Acc1);
pgw_record24(PGWRecord, Acc) ->
	pgw_record25(PGWRecord, Acc).
%% @hidden
pgw_record25(#{nBIFOMSupport := NBIFOMSupport} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"nBIFOMSupport">> => NBIFOMSupport},
	pgw_record26(PGWRecord, Acc1);
pgw_record25(PGWRecord, Acc) ->
	pgw_record26(PGWRecord, Acc).
%% @hidden
pgw_record26(#{nodeID := NodeID} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"nodeID">> => NodeID},
	pgw_record27(PGWRecord, Acc1);
pgw_record26(PGWRecord, Acc) ->
	pgw_record27(PGWRecord, Acc).
%% @hidden
pgw_record27(#{'p-GWAddress' := Address} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"p-GWAddress">> => cgf_lib:ip_address(Address)},
	pgw_record28(PGWRecord, Acc1);
pgw_record27(PGWRecord, Acc) ->
	pgw_record28(PGWRecord, Acc).
%% @hidden
pgw_record28(#{'p-GWPLMNIdentifier' := PGWPLMNIdentifier} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"p-GWPLMNIdentifier">> => cgf_lib:octet_string(PGWPLMNIdentifier)},
	pgw_record29(PGWRecord, Acc1);
pgw_record28(PGWRecord, Acc) ->
	pgw_record29(PGWRecord, Acc).
%% @hidden
pgw_record29(#{'p-GWiPv6AddressUsed' := Address} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"p-GWiPv6AddressUsed">> => cgf_lib:ip_address(Address)},
	pgw_record30(PGWRecord, Acc1);
pgw_record29(PGWRecord, Acc) ->
	pgw_record30(PGWRecord, Acc).
%% @hidden
pgw_record30(#{pDNConnectionChargingID := PDNConnChargingID} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"pDNConnectionChargingID">> => PDNConnChargingID},
	pgw_record31(PGWRecord, Acc1);
pgw_record30(PGWRecord, Acc) ->
	pgw_record31(PGWRecord, Acc).
%% @hidden
pgw_record31(#{pDPPDNTypeExtension := PDPPDNTypeExt} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"pDPPDNTypeExtension">> => PDPPDNTypeExt},
	pgw_record32(PGWRecord, Acc1);
pgw_record31(PGWRecord, Acc) ->
	pgw_record32(PGWRecord, Acc).
%% @hidden
pgw_record32(#{pSFurnishChargingInformation := PSFurnishChargingInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"pSFurnishChargingInformation">> => PSFurnishChargingInfo},
	pgw_record33(PGWRecord, Acc1);
pgw_record32(PGWRecord, Acc) ->
	pgw_record33(PGWRecord, Acc).
%% @hidden
pgw_record33(#{pdpPDNType := PDPType} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"pdpPDNType">> => PDPType},
	pgw_record34(PGWRecord, Acc1);
pgw_record33(PGWRecord, Acc) ->
	pgw_record34(PGWRecord, Acc).
%% @hidden
pgw_record34(#{presenceReportingAreaInfo := PresenceInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"presenceReportingAreaInfo">> => PresenceInfo},
	pgw_record35(PGWRecord, Acc1);
pgw_record34(PGWRecord, Acc) ->
	pgw_record35(PGWRecord, Acc).
%% @hidden
pgw_record35(#{rATType := RATType} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"rATType">> => RATType},
	pgw_record36(PGWRecord, Acc1);
pgw_record35(PGWRecord, Acc) ->
	pgw_record36(PGWRecord, Acc).
%% @hidden
pgw_record36(#{recordExtensions := PGWRecordExtensions} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => PGWRecordExtensions},
	pgw_record37(PGWRecord, Acc1);
pgw_record36(PGWRecord, Acc) ->
	pgw_record37(PGWRecord, Acc).
%% @hidden
pgw_record37(#{recordOpeningTime := RecordOpeningTime} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(RecordOpeningTime)},
	pgw_record38(PGWRecord, Acc1);
pgw_record37(PGWRecord, Acc) ->
	pgw_record38(PGWRecord, Acc).
%% @hidden
pgw_record38(#{recordOpeningTime := RecordOpeningTime} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(RecordOpeningTime)},
	pgw_record39(PGWRecord, Acc1);
pgw_record38(PGWRecord, Acc) ->
	pgw_record39(PGWRecord, Acc).
%% @hidden
pgw_record39(#{recordSequenceNumber := PGWRecordSequenceNumber} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"recordSequenceNumber">> => PGWRecordSequenceNumber},
	pgw_record40(PGWRecord, Acc1);
pgw_record39(PGWRecord, Acc) ->
	pgw_record40(PGWRecord, Acc).
%% @hidden
pgw_record40(#{recordType := PGWRecordType} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"recordType">> => PGWRecordType},
	pgw_record41(PGWRecord, Acc1);
pgw_record40(PGWRecord, Acc) ->
	pgw_record41(PGWRecord, Acc).
%% @hidden
pgw_record41(#{sCSASAddress := SCSASAddress} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"sCSASAddress">> => scsas(SCSASAddress)},
	pgw_record42(PGWRecord, Acc1);
pgw_record41(PGWRecord, Acc) ->
	pgw_record42(PGWRecord, Acc).
%% @hidden
pgw_record42(#{sGiPtPTunnellingMethod := SGiPtPTunnellingMethod} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"sGiPtPTunnellingMethod">> => SGiPtPTunnellingMethod},
	pgw_record43(PGWRecord, Acc1);
pgw_record42(PGWRecord, Acc) ->
	pgw_record43(PGWRecord, Acc).
%% @hidden
pgw_record43(#{served3gpp2MEID := Served3gpp2MEID} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"served3gpp2MEID">> => Served3gpp2MEID},
	pgw_record44(PGWRecord, Acc1);
pgw_record43(PGWRecord, Acc) ->
	pgw_record44(PGWRecord, Acc).
%% @hidden
pgw_record44(#{servedIMEI := ServedIMEI} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(ServedIMEI)},
	pgw_record45(PGWRecord, Acc1);
pgw_record44(PGWRecord, Acc) ->
	pgw_record45(PGWRecord, Acc).
%% @hidden
pgw_record45(#{servedIMSI := ServedIMSI} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	pgw_record46(PGWRecord, Acc1);
pgw_record45(PGWRecord, Acc) ->
	pgw_record46(PGWRecord, Acc).
%% @hidden
pgw_record46(#{servedMNNAI := ServedMNNAI} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servedMNNAI">> => ServedMNNAI},
	pgw_record47(PGWRecord, Acc1);
pgw_record46(PGWRecord, Acc) ->
	pgw_record47(PGWRecord, Acc).
%% @hidden
pgw_record47(#{servedMSISDN := ServedMSISDN} = PGWRecord, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	pgw_record48(PGWRecord, Acc1);
pgw_record47(PGWRecord, Acc) ->
	pgw_record48(PGWRecord, Acc).
%% @hidden
pgw_record48(#{servedPDPAddress
		:= {iPAddress, Address}} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servedPDPAddress">> => cgf_lib:ip_address(Address)},
	pgw_record49(PGWRecord, Acc1);
pgw_record48(PGWRecord, Acc) ->
	pgw_record49(PGWRecord, Acc).
%% @hidden
pgw_record49(#{servedPDPPDNAddressExt
		:= {iPAddress, Address}} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servedPDPPDNAddressExt">> => cgf_lib:ip_address(Address)},
	pgw_record50(PGWRecord, Acc1);
pgw_record49(PGWRecord, Acc) ->
	pgw_record50(PGWRecord, Acc).
%% @hidden
pgw_record50(#{servedPDPPDNAddress
		:= {iPAddress, Address}} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servedPDPPDNAddress">> => cgf_lib:ip_address(Address)},
	pgw_record51(PGWRecord, Acc1);
pgw_record50(PGWRecord, Acc) ->
	pgw_record51(PGWRecord, Acc).
%% @hidden
pgw_record51(#{servingNodeAddress := Addresses} = PGWRecord, Acc) ->
	ParsedAddresses = [cgf_lib:ip_address(Address)
			|| Address <- Addresses],
	Acc1 = Acc#{<<"servingNodeAddress">> => ParsedAddresses},
	pgw_record52(PGWRecord, Acc1);
pgw_record51(PGWRecord, Acc) ->
	pgw_record52(PGWRecord, Acc).
%% @hidden
pgw_record52(#{servingNodePLMNIdentifier := ServingNodePLMNId} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servingNodePLMNIdentifier">> => cgf_lib:octet_string(ServingNodePLMNId)},
	pgw_record53(PGWRecord, Acc1);
pgw_record52(PGWRecord, Acc) ->
	pgw_record53(PGWRecord, Acc).
%% @hidden
pgw_record53(#{servingNodeType := ServingNodeType} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servingNodeType">> => ServingNodeType},
	pgw_record54(PGWRecord, Acc1);
pgw_record53(PGWRecord, Acc) ->
	pgw_record54(PGWRecord, Acc).
%% @hidden
pgw_record54(#{servingNodeiPv6Address := Addresses} = PGWRecord, Acc) ->
	ParsedAddresses = [cgf_lib:ip_address(Address)
			|| Address <- Addresses],
	Acc1 = Acc#{<<"servingNodeiPv6Address">> => ParsedAddresses},
	pgw_record55(PGWRecord, Acc1);
pgw_record54(PGWRecord, Acc) ->
	pgw_record55(PGWRecord, Acc).
%% @hidden
pgw_record55(#{servingPLMNRateControl := ServingPLMNRateCtrl} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"servingPLMNRateControl">> => ServingPLMNRateCtrl},
	pgw_record56(PGWRecord, Acc1);
pgw_record55(PGWRecord, Acc) ->
	pgw_record56(PGWRecord, Acc).
%% @hidden
pgw_record56(#{startTime := StartTime} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"startTime">> => cgf_lib:bcd_date_time(StartTime)},
	pgw_record57(PGWRecord, Acc1);
pgw_record56(PGWRecord, Acc) ->
	pgw_record57(PGWRecord, Acc).
%% @hidden
pgw_record57(#{stopTime := StopTime} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"stopTime">> => cgf_lib:bcd_date_time(StopTime)},
	pgw_record58(PGWRecord, Acc1);
pgw_record57(PGWRecord, Acc) ->
	pgw_record58(PGWRecord, Acc).
%% @hidden
pgw_record58(#{tWANUserLocationInformation := TWANUserLocInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"tWANUserLocationInformation">> => TWANUserLocInfo},
	pgw_record59(PGWRecord, Acc1);
pgw_record58(PGWRecord, Acc) ->
	pgw_record59(PGWRecord, Acc).
%% @hidden
pgw_record59(#{threeGPP2UserLocationInformation := ThreeGPP2UserLocInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"threeGPP2UserLocationInformation">> => ThreeGPP2UserLocInfo},
	pgw_record60(PGWRecord, Acc1);
pgw_record59(PGWRecord, Acc) ->
	pgw_record60(PGWRecord, Acc).
%% @hidden
pgw_record60(#{threeGPPPSDataOffStatus := ThreeGPPPSDataOffStatus} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"threeGPPPSDataOffStatus">> => ThreeGPPPSDataOffStatus},
	pgw_record61(PGWRecord, Acc1);
pgw_record60(PGWRecord, Acc) ->
	pgw_record61(PGWRecord, Acc).
%% @hidden
pgw_record61(#{uNIPDUCPOnlyFlag := UNIPDUCPOnlyFlag} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"uNIPDUCPOnlyFlag">> => UNIPDUCPOnlyFlag},
	pgw_record62(PGWRecord, Acc1);
pgw_record61(PGWRecord, Acc) ->
	pgw_record62(PGWRecord, Acc).
%% @hidden
pgw_record62(#{uWANUserLocationInformation := UWANUserLocInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"uWANUserLocationInformation">> => UWANUserLocInfo},
	pgw_record63(PGWRecord, Acc1);
pgw_record62(PGWRecord, Acc) ->
	pgw_record64(PGWRecord, Acc).
%% @hidden
pgw_record63(#{userCSGInformation := UserCSGInfo} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"userCSGInformation">> => UserCSGInfo},
	pgw_record64(PGWRecord, Acc1);
pgw_record63(PGWRecord, Acc) ->
	pgw_record64(PGWRecord, Acc).
%% @hidden
pgw_record64(#{userLocationInfoTime := UserLocInfoTime} = PGWRecord, Acc) ->
	Acc1 = Acc#{<<"userLocationInfoTime">> => cgf_lib:bcd_date_time(UserLocInfoTime)},
	pgw_record65(PGWRecord, Acc1);
pgw_record64(PGWRecord, Acc) ->
	pgw_record65(PGWRecord, Acc).
%% @hidden
pgw_record65(#{userLocationInformation := UserLocInfo} = _PGWRecord, Acc) ->
	Acc#{<<"userLocationInformation">> => cgf_lib:octet_string(UserLocInfo)};
pgw_record65(_PGWRecord, Acc) ->
	Acc.

%% @hidden
epcqos_info(#{aPNAggregateMaxBitrateDL := APNAggregateMaxBitrateDL} = Record)
		when APNAggregateMaxBitrateDL > 0 ->
	Acc = #{<<"aPNAggregateMaxBitrateDL">> => APNAggregateMaxBitrateDL},
	epcqos_info1(Record, Acc);
epcqos_info(Record) ->
	epcqos_info1(Record, #{}).
%% @hidden
epcqos_info1(#{aPNAggregateMaxBitrateUL := APNAggregateMaxBitrateUL} = Record, Acc)
		when APNAggregateMaxBitrateUL > 0 ->
	Acc1 = Acc#{<<"aPNAggregateMaxBitrateUL">> => APNAggregateMaxBitrateUL},
	epcqos_info2(Record, Acc1);
epcqos_info1(Record, Acc) ->
	epcqos_info2(Record, Acc).
%% @hidden
epcqos_info2(#{aRP := ARP} = Record, Acc)
		when ARP > 0 ->
	Acc1 = Acc#{<<"aRP">> => ARP},
	epcqos_info3(Record, Acc1);
epcqos_info2(Record, Acc) ->
	epcqos_info3(Record, Acc).
%% @hidden
epcqos_info3(#{extendedAPNAMBRDL := ExtendedAPNAMBRDL} = Record, Acc)
		when ExtendedAPNAMBRDL > 0 ->
	Acc1 = Acc#{<<"extendedAPNAMBRDL">> => ExtendedAPNAMBRDL},
	epcqos_info4(Record, Acc1);
epcqos_info3(Record, Acc) ->
	epcqos_info4(Record, Acc).
%% @hidden
epcqos_info4(#{extendedAPNAMBRUL := ExtendedAPNAMBRUL} = Record, Acc)
		when ExtendedAPNAMBRUL > 0 ->
	Acc1 = Acc#{<<"extendedAPNAMBRUL">> => ExtendedAPNAMBRUL},
	epcqos_info5(Record, Acc1);
epcqos_info4(Record, Acc) ->
	epcqos_info5(Record, Acc).
%% @hidden
epcqos_info5(#{extendedGBRDL := ExtendedGBRDL} = Record, Acc)
		when ExtendedGBRDL > 0 ->
	Acc1 = Acc#{<<"extendedGBRDL">> => ExtendedGBRDL},
	epcqos_info6(Record, Acc1);
epcqos_info5(Record, Acc) ->
	epcqos_info6(Record, Acc).
%% @hidden
epcqos_info6(#{extendedGBRUL := ExtendedGBRUL} = Record, Acc)
		when ExtendedGBRUL > 0 ->
	Acc1 = Acc#{<<"extendedGBRUL">> => ExtendedGBRUL},
	epcqos_info7(Record, Acc1);
epcqos_info6(Record, Acc) ->
	epcqos_info7(Record, Acc).
%% @hidden
epcqos_info7(#{extendedMaxRequestedBWDL := ExtendedMaxReqBWDL} = Record, Acc)
		when ExtendedMaxReqBWDL > 0 ->
	Acc1 = Acc#{<<"extendedMaxRequestedBWDL">> => ExtendedMaxReqBWDL},
	epcqos_info8(Record, Acc1);
epcqos_info7(Record, Acc) ->
	epcqos_info8(Record, Acc).
%% @hidden
epcqos_info8(#{extendedMaxRequestedBWUL := ExtendedMaxReqBWUL} = Record, Acc)
		when ExtendedMaxReqBWUL > 0 ->
	Acc1 = Acc#{<<"extendedMaxRequestedBWUL">> => ExtendedMaxReqBWUL},
	epcqos_info9(Record, Acc1);
epcqos_info8(Record, Acc) ->
	epcqos_info9(Record, Acc).
%% @hidden
epcqos_info9(#{guaranteedBitrateDL := GuaranteedBitrateDL} = Record, Acc)
		when GuaranteedBitrateDL > 0 ->
	Acc1 = Acc#{<<"guaranteedBitrateDL">> => GuaranteedBitrateDL},
	epcqos_info10(Record, Acc1);
epcqos_info9(Record, Acc) ->
	epcqos_info10(Record, Acc).
%% @hidden
epcqos_info10(#{guaranteedBitrateUL := GuaranteedBitrateUL} = Record, Acc)
		when GuaranteedBitrateUL > 0 ->
	Acc1 = Acc#{<<"guaranteedBitrateUL">> => GuaranteedBitrateUL},
	epcqos_info11(Record, Acc1);
epcqos_info10(Record, Acc) ->
	epcqos_info11(Record, Acc).
%% @hidden
epcqos_info11(#{maxRequestedBandwithDL := MaxReqBandwidthDL} = Record, Acc)
		when MaxReqBandwidthDL > 0 ->
	Acc1 = Acc#{<<"maxRequestedBandwithDL">> => MaxReqBandwidthDL},
	epcqos_info12(Record, Acc1);
epcqos_info11(Record, Acc) ->
	epcqos_info12(Record, Acc).
%% @hidden
epcqos_info12(#{maxRequestedBandwithUL := MaxReqBandwidthUL} = Record, Acc)
		when MaxReqBandwidthUL > 0 ->
	Acc1 = Acc#{<<"maxRequestedBandwithUL">> => MaxReqBandwidthUL},
	epcqos_info13(Record, Acc1);
epcqos_info12(Record, Acc) ->
	epcqos_info13(Record, Acc).
%% @hidden
epcqos_info13(#{qCI := QCI} = _Record, Acc)
		when qCI > 0 ->
	Acc#{qCI => QCI};
epcqos_info13(_Record, Acc) ->
	Acc.

%% @hidden
ran_usage(#{chargingID := ChargingID} = Record)
		when length(ChargingID) > 0 ->
	Acc = #{<<"chargingID">> => ChargingID},
	ran_usage1(Record, Acc);
ran_usage(Record) ->
	ran_usage1(Record, #{}).
%% @hidden
ran_usage1(#{dataVolumeDownlink := DataVolumeDownlink} = Record, Acc)
		when DataVolumeDownlink > 0 ->
	Acc1 = Acc#{<<"dataVolumeDownlink">> => DataVolumeDownlink},
	ran_usage2(Record, Acc1);
ran_usage1(Record, Acc) ->
	ran_usage2(Record, Acc).
%% @hidden
ran_usage2(#{dataVolumeUplink := DataVolumeUplink} = Record, Acc)
		when DataVolumeUplink > 0 ->
	Acc1 = Acc#{<<"dataVolumeUplink">> => DataVolumeUplink},
	ran_usage3(Record, Acc1);
ran_usage2(Record, Acc) ->
	ran_usage3(Record, Acc).
%% @hidden
ran_usage3(#{rANEndTime := RANEndTime} = Record, Acc) ->
	Acc1 = Acc#{<<"rANEndTime">> => cgf_lib:bcd_date_time(RANEndTime)},
	ran_usage4(Record, Acc1);
ran_usage3(Record, Acc) ->
	ran_usage4(Record, Acc).
%% @hidden
ran_usage4(#{rANStartTime := RANStartTime} = Record, Acc) ->
	Acc1 = Acc#{<<"rANStartTime">> => cgf_lib:bcd_date_time(RANStartTime)},
	ran_usage5(Record, Acc1);
ran_usage4(Record, Acc) ->
	ran_usage5(Record, Acc).
%% @hidden
ran_usage5(#{secondaryRATType := SecondaryRATType} = _Record, Acc) ->
	Acc#{secondaryRATType => SecondaryRATType};
ran_usage5(_Record, Acc) ->
	Acc.

service_data(#{aDCRuleBaseName := ADCRuleBaseName} = ServiceData) ->
	Acc = #{<<"aDCRuleBaseName">> => ADCRuleBaseName},
	service_data1(ServiceData, Acc);
service_data(ServiceData) ->
	service_data1(ServiceData, #{}).
%% @hidden
service_data1(#{aFServiceDataInformation := AFServiceDataInformation} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"aFServiceDataInformation">> => AFServiceDataInformation},
	service_data2(ServiceData, Acc1);
service_data1(ServiceData, Acc) ->
	service_data2(ServiceData, Acc).
%% @hidden
service_data2(#{aPNRateControl := APNRateControl} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"aPNRateControl">> => APNRateControl},
	service_data3(ServiceData, Acc1);
service_data2(ServiceData, Acc) ->
	service_data3(ServiceData, Acc).
%% @hidden
service_data3(#{applicationServiceProviderIdentity :=
		AppServiceProviderIdentity} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"applicationServiceProviderIdentity">> => AppServiceProviderIdentity},
	service_data4(ServiceData, Acc1);
service_data3(ServiceData, Acc) ->
	service_data4(ServiceData, Acc).
%% @hidden
service_data4(#{chargingRuleBaseName := ChargingRuleBaseName} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"chargingRuleBaseName">> => ChargingRuleBaseName},
	service_data5(ServiceData, Acc1);
service_data4(ServiceData, Acc) ->
	service_data5(ServiceData, Acc).
%% @hidden
service_data5(#{datavolumeFBCDownlink := DatavolumeFBCDownlink} = ServiceData, Acc)
		when DatavolumeFBCDownlink > 0 ->
	Acc1 = Acc#{<<"datavolumeFBCDownlink">> => DatavolumeFBCDownlink},
	service_data6(ServiceData, Acc1);
service_data5(ServiceData, Acc) ->
	service_data6(ServiceData, Acc).
%% @hidden
service_data6(#{datavolumeFBCUplink := DatavolumeFBCUplink} = ServiceData, Acc)
		when DatavolumeFBCUplink > 0 ->
	Acc1 = Acc#{<<"datavolumeFBCUplink">> => DatavolumeFBCUplink},
	service_data7(ServiceData, Acc1);
service_data6(ServiceData, Acc) ->
	service_data7(ServiceData, Acc).
%% @hidden
service_data7(#{eventBasedChargingInformation :=
		EventBasedChargingInformation} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"eventBasedChargingInformation">> =>
			event_charging_info(EventBasedChargingInformation)},
	service_data8(ServiceData, Acc1);
service_data7(ServiceData, Acc) ->
	service_data8(ServiceData, Acc).
%% @hidden
service_data8(#{failureHandlingContinue := FailureHandlingContinue} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"failureHandlingContinue">> => FailureHandlingContinue},
	service_data9(ServiceData, Acc1);
service_data8(ServiceData, Acc) ->
	service_data9(ServiceData, Acc).
%% @hidden
service_data9(#{listOfPresenceReportingAreaInformation
		:= ListOfPresenceReportingAreaInformation} = ServiceData, Acc) ->
		Acc1 = Acc#{<<"listOfPresenceReportingAreaInformation">> =>
				ListOfPresenceReportingAreaInformation},
	service_data10(ServiceData, Acc1);
service_data9(ServiceData, Acc) ->
	service_data10(ServiceData, Acc).
%% @hidden
service_data10(#{localSequenceNumber := LocalSequenceNumber} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"localSequenceNumber">> => LocalSequenceNumber},
	service_data11(ServiceData, Acc1);
service_data10(ServiceData, Acc) ->
	service_data11(ServiceData, Acc).
%% @hidden
service_data11(#{pSFurnishChargingInformation := PSFurnishChargingInfo} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"pSFurnishChargingInformation">> => PSFurnishChargingInfo},
	service_data12(ServiceData, Acc1);
service_data11(ServiceData, Acc) ->
	service_data12(ServiceData, Acc).
%% @hidden
service_data12(#{presenceReportingAreaStatus := PresenceReportingAreaStatus} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"presenceReportingAreaStatus">> => PresenceReportingAreaStatus},
	service_data13(ServiceData, Acc1);
service_data12(ServiceData, Acc) ->
	service_data13(ServiceData, Acc).
%% @hidden
service_data13(#{qoSInformationNeg := QoSInformationNeg} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"qoSInformationNeg">> => epcqos_info(QoSInformationNeg)},
	service_data14(ServiceData, Acc1);
service_data13(ServiceData, Acc) ->
	service_data14(ServiceData, Acc).
%% @hidden
service_data14(#{rATType := RATType} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"rATType">> => RATType},
	service_data15(ServiceData, Acc1);
service_data14(ServiceData, Acc) ->
	service_data15(ServiceData, Acc).
%% @hidden
service_data15(#{ratingGroup := RatingGroup} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"ratingGroup">> => RatingGroup},
	service_data16(ServiceData, Acc1);
service_data15(ServiceData, Acc) ->
	service_data16(ServiceData, Acc).
%% @hidden
service_data16(#{relatedChangeOfServiceCondition := RelatedChangeOfServiceCondition} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"relatedChangeOfServiceCondition">> => RelatedChangeOfServiceCondition},
	service_data17(ServiceData, Acc1);
service_data16(ServiceData, Acc) ->
	service_data17(ServiceData, Acc).
%% @hidden
service_data17(#{resultCode := ResultCode} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"resultCode">> => ResultCode},
	service_data18(ServiceData, Acc1);
service_data17(ServiceData, Acc) ->
	service_data18(ServiceData, Acc).
%% @hidden
service_data18(#{serviceConditionChange := ServiceConditionChange} = ServiceData, Acc)
		when length(ServiceConditionChange) > 0 ->
	Acc1 = Acc#{<<"serviceConditionChange">> => ServiceConditionChange},
	service_data19(ServiceData, Acc1);
service_data18(ServiceData, Acc) ->
	service_data19(ServiceData, Acc).
%% @hidden
service_data19(#{serviceIdentifier := ServiceIdentifier} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"serviceIdentifier">> => ServiceIdentifier},
	service_data20(ServiceData, Acc1);
service_data19(ServiceData, Acc) ->
	service_data20(ServiceData, Acc).
%% @hidden
service_data20(#{serviceSpecificInfo := ServiceSpecificInfo} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"serviceSpecificInfo">> => ServiceSpecificInfo},
	service_data21(ServiceData, Acc1);
service_data20(ServiceData, Acc) ->
	service_data21(ServiceData, Acc).
%% @hidden
service_data21(#{servingNodeAddress := Addresses} = ServiceData, Acc) ->
	ParsedAddresses = [cgf_lib:ip_address(Address)
			|| Address <- Addresses],
	Acc1 = Acc#{<<"servingNodeAddress">> => ParsedAddresses},
	service_data22(ServiceData, Acc1);
service_data21(ServiceData, Acc) ->
	service_data22(ServiceData, Acc).
%% @hidden
service_data22(#{servingPLMNRateControl := ServingPLMNRateCtrl} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"servingPLMNRateControl">> => ServingPLMNRateCtrl},
	service_data23(ServiceData, Acc1);
service_data22(ServiceData, Acc) ->
	service_data23(ServiceData, Acc).
%% @hidden
service_data23(#{sponsorIdentity := SponsorIdentity} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"sponsorIdentity">> => SponsorIdentity},
	service_data24(ServiceData, Acc1);
service_data23(ServiceData, Acc) ->
	service_data24(ServiceData, Acc).
%% @hidden
service_data24(#{tWANUserLocationInformation := TWANUserLocInfo} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"tWANUserLocationInformation">> => cgf_lib:octet_string(TWANUserLocInfo)},
	service_data25(ServiceData, Acc1);
service_data24(ServiceData, Acc) ->
	service_data25(ServiceData, Acc).
%% @hidden
service_data25(#{threeGPP2UserLocationInformation := ThreeGPP2UserLocInfo} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"threeGPP2UserLocationInformation">> => cgf_lib:octet_string(ThreeGPP2UserLocInfo)},
	service_data26(ServiceData, Acc1);
service_data25(ServiceData, Acc) ->
	service_data26(ServiceData, Acc).
%% @hidden
service_data26(#{threeGPPPSDataOffStatus := ThreeGPPPSDataOffStatus} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"threeGPPPSDataOffStatus">> => ThreeGPPPSDataOffStatus},
	service_data27(ServiceData, Acc1);
service_data26(ServiceData, Acc) ->
	service_data27(ServiceData, Acc).
%% @hidden
service_data27(#{timeOfFirstUsage := TimeOfFirstUsage} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"timeOfFirstUsage">> => cgf_lib:bcd_date_time(TimeOfFirstUsage)},
	service_data28(ServiceData, Acc1);
service_data27(ServiceData, Acc) ->
	service_data28(ServiceData, Acc).
%% @hidden
service_data28(#{timeOfLastUsage := TimeOfLastUsage} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"timeOfLastUsage">> => cgf_lib:bcd_date_time(TimeOfLastUsage)},
	service_data29(ServiceData, Acc1);
service_data28(ServiceData, Acc) ->
	service_data29(ServiceData, Acc).
%% @hidden
service_data29(#{timeOfReport := TimeOfReport} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"timeOfReport">> => cgf_lib:bcd_date_time(TimeOfReport)},
	service_data30(ServiceData, Acc1);
service_data29(ServiceData, Acc) ->
	service_data30(ServiceData, Acc).
%% @hidden
service_data30(#{timeQuotaMechanism := TimeQuotaMechanism} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"timeQuotaMechanism">> => TimeQuotaMechanism},
	service_data31(ServiceData, Acc1);
service_data30(ServiceData, Acc) ->
	service_data31(ServiceData, Acc).
%% @hidden
service_data31(#{timeUsage := TimeUsage} = ServiceData, Acc)
		when TimeUsage > 0 ->
	Acc1 = Acc#{<<"timeUsage">> => TimeUsage},
	service_data32(ServiceData, Acc1);
service_data31(ServiceData, Acc) ->
	service_data32(ServiceData, Acc).
%% @hidden
service_data32(#{trafficSteeringPolicyIDDownlink := TrafficSteeringPolicyIDDownlink} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"trafficSteeringPolicyIDDownlink">> => TrafficSteeringPolicyIDDownlink},
	service_data33(ServiceData, Acc1);
service_data32(ServiceData, Acc) ->
	service_data33(ServiceData, Acc).
%% @hidden
service_data33(#{trafficSteeringPolicyIDUplink := TrafficSteeringPolicyIDUplink} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"trafficSteeringPolicyIDUplink">> => TrafficSteeringPolicyIDUplink},
	service_data34(ServiceData, Acc1);
service_data33(ServiceData, Acc) ->
	service_data34(ServiceData, Acc).
%% @hidden
service_data34(#{uWANUserLocationInformation := UWANUserLocInfo} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"uWANUserLocationInformation">> => cgf_lib:octet_string(UWANUserLocInfo)},
	service_data35(ServiceData, Acc1);
service_data34(ServiceData, Acc) ->
	service_data35(ServiceData, Acc).
%% @hidden
service_data35(#{userCSGInformation := UserCSGInfo} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"userCSGInformation">> => UserCSGInfo},
	service_data36(ServiceData, Acc1);
service_data35(ServiceData, Acc) ->
	service_data36(ServiceData, Acc).
%% @hidden
service_data36(#{userLocationInformation := UserLocInfo} = ServiceData, Acc) ->
	Acc1 = Acc#{<<"userLocationInformation">> => cgf_lib:octet_string(UserLocInfo)},
	service_data37(ServiceData, Acc1);
service_data36(ServiceData, Acc) ->
	service_data37(ServiceData, Acc).
%% @hidden
service_data37(#{voLTEInformation := VoLTEInfo} = _ServiceData, Acc) ->
	Acc#{<<"voLTEInformation">> => VoLTEInfo};
service_data37(_ServiceData, Acc) ->
	Acc.

%% @hidden
mo_exception(#{counterTimestamp := CounterTimestamp} = MOExceptionDataCounter) ->
	Acc = #{<<"counterTimestamp">> => cgf_lib:bcd_date_time(CounterTimestamp)},
	mo_exception1(MOExceptionDataCounter, Acc);
mo_exception(MOExceptionDataCounter) ->
	mo_exception1(MOExceptionDataCounter, #{}).
%% @hidden
mo_exception1(#{counterValue := CounterValue} = _MOExceptionDataCounter, Acc) ->
	Acc#{<<"counterValue">> => CounterValue};
mo_exception1(_MOExceptionDataCounter, Acc) ->
	Acc.

%% @hidden
scsas(#{sCSRealm := SCSRealm} = SCSAS) ->
	Acc = #{<<"sCSRealm">> => binary_to_list(SCSRealm)},
	scsas1(SCSAS, Acc);
scsas(SCSAS) ->
	scsas1(SCSAS, #{}).
%% @hidden
scsas1(#{sCSAddress := Address} = _SCSAS, Acc) ->
	Acc#{<<"sCSAddress">> => cgf_lib:ip_address(Address)};
scsas1(_SCSAS, Acc) ->
	Acc.

%% @hidden
event_charging_info(#{eventTimeStamps := EventTimeStamps} = Info) ->
	ParsedTime = [cgf_lib:bcd_date_time(TimeStamp) ||
			{_, {_, TimeStamp}} <- EventTimeStamps],
	Acc = #{<<"eventTimeStamps">> => ParsedTime},
	event_charging_info1(Info, Acc);
event_charging_info(Info) ->
	event_charging_info1(Info, #{}).
%% @hidden
event_charging_info1(#{numberOfEvents := NumberOfEvents} = _Info, Acc) ->
	Acc#{<<"numberOfEvents">> => NumberOfEvents};
event_charging_info1(_Info, Acc) ->
	Acc.

%% @hidden
sgsn_mmr(#{cAMELInformationMM := CAMELInfo} = SGSNMMRRecord) ->
	% Acc = #{<<"cAMELInformationMM">> => camel_info_mm(CAMELInfo)},
	sgsn_mmr1(SGSNMMRRecord, #{});
sgsn_mmr(SGSNMMRRecord) ->
	sgsn_mmr1(SGSNMMRRecord, #{}).
%% @hidden
sgsn_mmr1(#{cNOperatorSelectionEnt := CNOperatorSelectionEnt} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"cNOperatorSelectionEnt">> => CNOperatorSelectionEnt},
	sgsn_mmr2(SGSNMMRRecord, Acc1);
sgsn_mmr1(SGSNMMRRecord, Acc) ->
	sgsn_mmr2(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr2(#{causeForRecClosing := CauseForRecClosing} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"causeForRecClosing">> => CauseForRecClosing},
	sgsn_mmr3(SGSNMMRRecord, Acc1);
sgsn_mmr2(SGSNMMRRecord, Acc) ->
	sgsn_mmr3(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr3(#{cellIdentifier := CellIdentifier} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"cellIdentifier">> => CellIdentifier},
	sgsn_mmr4(SGSNMMRRecord, Acc1);
sgsn_mmr3(SGSNMMRRecord, Acc) ->
	sgsn_mmr4(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr4(#{cellPLMNId := CellPLMNId} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"cellPLMNId">> => CellPLMNId},
	sgsn_mmr5(SGSNMMRRecord, Acc1);
sgsn_mmr4(SGSNMMRRecord, Acc) ->
	sgsn_mmr5(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr5(#{chChSelectionMode := ChChSelectionMode} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"chChSelectionMode">> => ChChSelectionMode},
	sgsn_mmr6(SGSNMMRRecord, Acc1);
sgsn_mmr5(SGSNMMRRecord, Acc) ->
	sgsn_mmr6(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr6(#{changeLocation := ChangeLocation} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"changeLocation">> => change_location(ChangeLocation)}, %% Array
	sgsn_mmr7(SGSNMMRRecord, Acc1);
sgsn_mmr6(SGSNMMRRecord, Acc) ->
	sgsn_mmr7(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr7(#{chargingCharacteristics := CC} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"chargingCharacteristics">> => cgf_lib:octet_string(CC)},
	sgsn_mmr8(SGSNMMRRecord, Acc1);
sgsn_mmr7(SGSNMMRRecord, Acc) ->
	sgsn_mmr8(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr8(#{diagnostics := Diagnostics} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	sgsn_mmr9(SGSNMMRRecord, Acc1);
sgsn_mmr8(SGSNMMRRecord, Acc) ->
	sgsn_mmr9(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr9(#{duration := Duration} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"duration">> => Duration},
	sgsn_mmr10(SGSNMMRRecord, Acc1);
sgsn_mmr9(SGSNMMRRecord, Acc) ->
	sgsn_mmr10(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr10(#{localSequenceNumber := LocalSequenceNumber} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"localSequenceNumber">> => LocalSequenceNumber},
	sgsn_mmr11(SGSNMMRRecord, Acc1);
sgsn_mmr10(SGSNMMRRecord, Acc) ->
	sgsn_mmr11(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr11(#{locationAreaCode := LocationAreaCode} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"locationAreaCode">> => cgf_lib:octet_string(LocationAreaCode)},
	sgsn_mmr12(SGSNMMRRecord, Acc1);
sgsn_mmr11(SGSNMMRRecord, Acc) ->
	sgsn_mmr12(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr12(#{msNetworkCapability
		:= MSNetworkCapability} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"msNetworkCapability">>
			=> cgf_lib:octet_string(MSNetworkCapability)},
	sgsn_mmr13(SGSNMMRRecord, Acc1);
sgsn_mmr12(SGSNMMRRecord, Acc) ->
	sgsn_mmr13(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr13(#{nodeID := NodeID} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"nodeID">> => NodeID},
	sgsn_mmr14(SGSNMMRRecord, Acc1);
sgsn_mmr13(SGSNMMRRecord, Acc) ->
	sgsn_mmr14(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr14(#{rATType := RATType} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"rATType">> => RATType},
	sgsn_mmr15(SGSNMMRRecord, Acc1);
sgsn_mmr14(SGSNMMRRecord, Acc) ->
	sgsn_mmr15(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr15(#{recordExtensions := RecordExtensions} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => RecordExtensions},
	sgsn_mmr16(SGSNMMRRecord, Acc1);
sgsn_mmr15(SGSNMMRRecord, Acc) ->
	sgsn_mmr16(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr16(#{recordOpeningTime := RecordOpeningTime} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(RecordOpeningTime)},
	sgsn_mmr17(SGSNMMRRecord, Acc1);
sgsn_mmr16(SGSNMMRRecord, Acc) ->
	sgsn_mmr17(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr17(#{recordSequenceNumber := RecordSequenceNumber} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"recordSequenceNumber">> => RecordSequenceNumber},
	sgsn_mmr18(SGSNMMRRecord, Acc1);
sgsn_mmr17(SGSNMMRRecord, Acc) ->
	sgsn_mmr18(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr18(#{recordType := RecordType} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"recordType">> => RecordType},
	sgsn_mmr19(SGSNMMRRecord, Acc1);
sgsn_mmr18(SGSNMMRRecord, Acc) ->
	sgsn_mmr19(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr19(#{routingArea := RoutingArea} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"routingArea">> => cgf_lib:octet_string(RoutingArea)},
	sgsn_mmr20(SGSNMMRRecord, Acc1);
sgsn_mmr19(SGSNMMRRecord, Acc) ->
	sgsn_mmr20(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr20(#{servedIMEI := ServedIMEI} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(ServedIMEI)},
	sgsn_mmr21(SGSNMMRRecord, Acc1);
sgsn_mmr20(SGSNMMRRecord, Acc) ->
	sgsn_mmr21(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr21(#{servedIMSI := ServedIMSI} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	sgsn_mmr22(SGSNMMRRecord, Acc1);
sgsn_mmr21(SGSNMMRRecord, Acc) ->
	sgsn_mmr22(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr22(#{servedMSISDN := ServedMSISDN} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"servedMSISDN">> => cgf_lib:tbcd(ServedMSISDN)},
	sgsn_mmr23(SGSNMMRRecord, Acc1);
sgsn_mmr22(SGSNMMRRecord, Acc) ->
	sgsn_mmr23(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr23(#{servingNodePLMNIdentifier := Identifer} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"servingNodePLMNIdentifier">> => cgf_lib:octet_string(Identifer)},
	sgsn_mmr24(SGSNMMRRecord, Acc1);
sgsn_mmr23(SGSNMMRRecord, Acc) ->
	sgsn_mmr24(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr24(#{sgsnAddress := Address} = SGSNMMRRecord, Acc) ->
	Acc1 = Acc#{<<"sgsnAddress">> => cgf_lib:ip_address(Address)},
	sgsn_mmr25(SGSNMMRRecord, Acc1);
sgsn_mmr24(SGSNMMRRecord, Acc) ->
	sgsn_mmr25(SGSNMMRRecord, Acc).
%% @hidden
sgsn_mmr25(#{sgsnChange := SgsnChange} = _SGSNMMRRecord, Acc) ->
	Acc#{<<"sgsnChange">> => SgsnChange};
sgsn_mmr25(_SGSNMMRRecord, Acc) ->
	Acc.

%% @hidden
change_location(#{cellId := CellId} = ChangeLocation) ->
	Acc = #{<<"cellId">> => CellId},
	change_location1(ChangeLocation, Acc);
change_location(ChangeLocation) ->
	change_location1(ChangeLocation, #{}).
%% @hidden
change_location1(#{changeTime := ChangeTime} = ChangeLocation, Acc) ->
	Acc1 = Acc#{<<"changeTime">> => cgf_lib:bcd_date_time(ChangeTime)},
	change_location2(ChangeLocation, Acc1);
change_location1(ChangeLocation, Acc) ->
	change_location2(ChangeLocation, Acc).
%% @hidden
change_location2(#{locationAreaCode := LocationAreaCode} = ChangeLocation, Acc) ->
	Acc1 = Acc#{<<"locationAreaCode">> => cgf_lib:octet_string(LocationAreaCode)},
	change_location3(ChangeLocation, Acc1);
change_location2(ChangeLocation, Acc) ->
	change_location3(ChangeLocation, Acc).
%% @hidden
change_location3(#{'mCC-MNC' := MCCMNC} = ChangeLocation, Acc) ->
	Acc1 = Acc#{<<"mCC-MNC">> => MCCMNC},
	change_location4(ChangeLocation, Acc1);
change_location3(ChangeLocation, Acc) ->
	change_location4(ChangeLocation, Acc).
%% @hidden
change_location4(#{routingAreaCode := RoutingAreaCode} = _ChangeLocation, Acc) ->
	Acc#{<<"routingAreaCode">> => RoutingAreaCode};
change_location4(_ChangeLocation, Acc) ->
	Acc.

%% @hidden
enhanced_dianostics(#{rANNASCause := RanNasCause}) ->
	Causes = [cgf_lib:octet_string(Cause) || Cause <- RanNasCause],
	#{<<"rANNASCause">> => Causes}.

%% @hidden
sgsn_smo(#{servedIMSI := IMSI} = Record) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:tbcd(IMSI)},
	sgsn_smo1(Record, Acc);
sgsn_smo(Record) ->
	sgsn_smo1(Record, #{}).
%% @hidden
sgsn_smo1(#{servedIMEI := IMEI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(IMEI)},
	sgsn_smo2(Record, Acc1);
sgsn_smo1(Record, Acc) ->
	sgsn_smo2(Record, Acc).
%% @hidden
sgsn_smo2(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	sgsn_smo3(Record, Acc1);
sgsn_smo2(Record, Acc) ->
	sgsn_smo3(Record, Acc).
%% @hidden
sgsn_smo3(#{msNetworkCapability := Capability} = Record, Acc) ->
	Acc1 = Acc#{<<"msNetworkCapability">> => cgf_lib:octet_string(Capability)},
	sgsn_smo4(Record, Acc1);
sgsn_smo3(Record, Acc) ->
	sgsn_smo4(Record, Acc).
%% @hidden
sgsn_smo4(#{serviceCenter := SC} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceCenter">> => cgf_lib:bcd_dn(SC)},
	sgsn_smo5(Record, Acc1);
sgsn_smo4(Record, Acc) ->
	sgsn_smo5(Record, Acc).
%% @hidden
sgsn_smo5(#{recordingEntity := Entity} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(Entity)},
	sgsn_smo6(Record, Acc1);
sgsn_smo5(Record, Acc) ->
	sgsn_smo6(Record, Acc).
%% @hidden
sgsn_smo6(#{locationArea := Area} = Record, Acc) ->
	Acc1 = Acc#{<<"locationArea">> => cgf_lib:octet_string(Area)},
	sgsn_smo7(Record, Acc1);
sgsn_smo6(Record, Acc) ->
	sgsn_smo7(Record, Acc).
%% @hidden
sgsn_smo7(#{routingArea := Area} = Record, Acc) ->
	Acc1 = Acc#{<<"routingArea">> => cgf_lib:octet_string(Area)},
	sgsn_smo8(Record, Acc1);
sgsn_smo7(Record, Acc) ->
	sgsn_smo8(Record, Acc).
%% @hidden
sgsn_smo8(#{cellIdentifier := CI} = Record, Acc) ->
	Acc1 = Acc#{<<"cellIdentifier">> => cgf_lib:octet_string(CI)},
	sgsn_smo9(Record, Acc1);
sgsn_smo8(Record, Acc) ->
	sgsn_smo9(Record, Acc).
%% @hidden
sgsn_smo9(#{messageReference := Ref} = Record, Acc) ->
	Acc1 = Acc#{<<"messageReference">> => cgf_lib:octet_string(Ref)},
	sgsn_smo10(Record, Acc1);
sgsn_smo9(Record, Acc) ->
	sgsn_smo10(Record, Acc).
%% @hidden
sgsn_smo10(#{eventTimeStamp := TS} = Record, Acc) ->
	Acc1 = Acc#{<<"eventTimeStamp">> => cgf_lib:bcd_date_time(TS)},
	sgsn_smo11(Record, Acc1);
sgsn_smo10(Record, Acc) ->
	sgsn_smo11(Record, Acc).
%% @hidden
sgsn_smo11(#{smsResult := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"smsResult">> => cgf_lib:diagnostics(Diagnostics)},
	sgsn_smo12(Record, Acc1);
sgsn_smo11(Record, Acc) ->
	sgsn_smo12(Record, Acc).
%% @hidden
sgsn_smo12(#{nodeID := NodeID} = Record, Acc) ->
	Acc1 = Acc#{<<"nodeID">> => NodeID},
	sgsn_smo13(Record, Acc1);
sgsn_smo12(Record, Acc) ->
	sgsn_smo13(Record, Acc).
%% @hidden
sgsn_smo13(#{localSequenceNumber := Sequence} = Record, Acc) ->
	Acc1 = Acc#{<<"localSequenceNumber">> => Sequence},
	sgsn_smo14(Record, Acc1);
sgsn_smo13(Record, Acc) ->
	sgsn_smo14(Record, Acc).
%% @hidden
sgsn_smo14(#{chargingCharacteristics := CC} = Record, Acc) ->
	Acc1 = Acc#{<<"chargingCharacteristics">> => cgf_lib:octet_string(CC)},
	sgsn_smo15(Record, Acc1);
sgsn_smo14(Record, Acc) ->
	sgsn_smo15(Record, Acc).
%% @hidden
sgsn_smo15(#{rATType := RATType} = Record, Acc) ->
	Acc1 = Acc#{<<"rATType">> => RATType},
	sgsn_smo16(Record, Acc1);
sgsn_smo15(Record, Acc) ->
	sgsn_smo16(Record, Acc).
%% @hidden
sgsn_smo16(#{destinationNumber := DN} = Record, Acc) ->
	Acc1 = Acc#{<<"SmsTpDestinationNumber">> => cgf_lib:bcd_dn(DN)},
	sgsn_smo17(Record, Acc1);
sgsn_smo16(Record, Acc) ->
	sgsn_smo17(Record, Acc).
%% @hidden
sgsn_smo17(#{cAMELInformationSMS := CAMELInfo} = Record, Acc) ->
	% Acc1 = Acc#{<<"cAMELInformationSMS">> => camel_info_sms(CAMELInfo)},
	sgsn_smo18(Record, Acc);
sgsn_smo17(Record, Acc) ->
	sgsn_smo18(Record, Acc).
%% @hidden
sgsn_smo18(#{chChSelectionMode := Mode} = Record, Acc) ->
	Acc1 = Acc#{<<"chChSelectionMode">> => Mode},
	sgsn_smo19(Record, Acc1);
sgsn_smo18(Record, Acc) ->
	sgsn_smo19(Record, Acc).
%% @hidden
sgsn_smo19(#{servingNodeType := Type} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodeType">> => Type},
	sgsn_smo20(Record, Acc1);
sgsn_smo19(Record, Acc) ->
	sgsn_smo20(Record, Acc).
%% @hidden
sgsn_smo20(#{servingNodeAddress := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodeAddress">> => cgf_lib:ip_address(Address)},
	sgsn_smo21(Record, Acc1);
sgsn_smo20(Record, Acc) ->
	sgsn_smo21(Record, Acc).
%% @hidden
sgsn_smo21(#{servingNodeiPv6Address := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodeiPv6Address">> => cgf_lib:ip_address(Address)},
	sgsn_smo22(Record, Acc1);
sgsn_smo21(Record, Acc) ->
	sgsn_smo22(Record, Acc).
%% @hidden
sgsn_smo22(#{mMEName := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"mMEName">> => Address},
	sgsn_smo23(Record, Acc1);
sgsn_smo22(Record, Acc) ->
	sgsn_smo23(Record, Acc).
%% @hidden
sgsn_smo23(#{mMERealm := Realm} = Record, Acc) ->
	Acc1 = Acc#{<<"mMERealm">> => Realm},
	sgsn_smo24(Record, Acc1);
sgsn_smo23(Record, Acc) ->
	sgsn_smo24(Record, Acc).
%% @hidden
sgsn_smo24(#{userLocationInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"userLocationInformation">> => cgf_lib:octet_string(Info)},
	sgsn_smo25(Record, Acc1);
sgsn_smo24(Record, Acc) ->
	sgsn_smo25(Record, Acc).
%% @hidden
sgsn_smo25(#{retransmission := _} = Record, Acc) ->
	Acc1 = Acc#{<<"retransmission">> => undefined},
	sgsn_smo26(Record, Acc1);
sgsn_smo25(Record, Acc) ->
	sgsn_smo26(Record, Acc).
%% @hidden
sgsn_smo26(#{servingNodePLMNIdentifier := PLMN} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodePLMNIdentifier">> => cgf_lib:octet_string(PLMN)},
	sgsn_smo27(Record, Acc1);
sgsn_smo26(Record, Acc) ->
	sgsn_smo27(Record, Acc).
%% @hidden
sgsn_smo27(#{userLocationInfoTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"userLocationInfoTime">> => cgf_lib:bcd_date_time(Time)},
	sgsn_smo28(Record, Acc1);
sgsn_smo27(Record, Acc) ->
	sgsn_smo28(Record, Acc).
%% @hidden
sgsn_smo28(#{cNOperatorSelectionEnt := Entity}, Acc) ->
	Acc#{<<"cNOperatorSelectionEnt">> => Entity};
sgsn_smo28(_Record, Acc) ->
	Acc.

%% @hidden
sgsn_smt(#{servedIMSI := IMSI} = Record) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:tbcd(IMSI)},
	sgsn_smt1(Record, Acc);
sgsn_smt(Record) ->
	sgsn_smt1(Record, #{}).
%% @hidden
sgsn_smt1(#{servedIMEI := IMEI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(IMEI)},
	sgsn_smt2(Record, Acc1);
sgsn_smt1(Record, Acc) ->
	sgsn_smt2(Record, Acc).
%% @hidden
sgsn_smt2(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	sgsn_smt3(Record, Acc1);
sgsn_smt2(Record, Acc) ->
	sgsn_smt3(Record, Acc).
%% @hidden
sgsn_smt3(#{msNetworkCapability := Capability} = Record, Acc) ->
	Acc1 = Acc#{<<"msNetworkCapability">> => cgf_lib:octet_string(Capability)},
	sgsn_smt4(Record, Acc1);
sgsn_smt3(Record, Acc) ->
	sgsn_smt4(Record, Acc).
%% @hidden
sgsn_smt4(#{serviceCenter := SC} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceCenter">> => cgf_lib:bcd_dn(SC)},
	sgsn_smt5(Record, Acc1);
sgsn_smt4(Record, Acc) ->
	sgsn_smt5(Record, Acc).
%% @hidden
sgsn_smt5(#{recordingEntity := Entity} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(Entity)},
	sgsn_smt6(Record, Acc1);
sgsn_smt5(Record, Acc) ->
	sgsn_smt6(Record, Acc).
%% @hidden
sgsn_smt6(#{locationArea := Area} = Record, Acc) ->
	Acc1 = Acc#{<<"locationArea">> => cgf_lib:octet_string(Area)},
	sgsn_smt7(Record, Acc1);
sgsn_smt6(Record, Acc) ->
	sgsn_smt7(Record, Acc).
%% @hidden
sgsn_smt7(#{routingArea := Area} = Record, Acc) ->
	Acc1 = Acc#{<<"routingArea">> => cgf_lib:octet_string(Area)},
	sgsn_smt8(Record, Acc1);
sgsn_smt7(Record, Acc) ->
	sgsn_smt8(Record, Acc).
%% @hidden
sgsn_smt8(#{cellIdentifier := CI} = Record, Acc) ->
	Acc1 = Acc#{<<"cellIdentifier">> => cgf_lib:octet_string(CI)},
	sgsn_smt9(Record, Acc1);
sgsn_smt8(Record, Acc) ->
	sgsn_smt9(Record, Acc).
%% @hidden
sgsn_smt9(#{eventTimeStamp := TS} = Record, Acc) ->
	Acc1 = Acc#{<<"eventTimeStamp">> => cgf_lib:bcd_date_time(TS)},
	sgsn_smt10(Record, Acc1);
sgsn_smt9(Record, Acc) ->
	sgsn_smt10(Record, Acc).
%% @hidden
sgsn_smt10(#{smsResult := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"smsResult">> => cgf_lib:diagnostics(Diagnostics)},
	sgsn_smt11(Record, Acc1);
sgsn_smt10(Record, Acc) ->
	sgsn_smt11(Record, Acc).
%% @hidden
sgsn_smt11(#{nodeID := NodeID} = Record, Acc) ->
	Acc1 = Acc#{<<"nodeID">> => NodeID},
	sgsn_smt12(Record, Acc1);
sgsn_smt11(Record, Acc) ->
	sgsn_smt12(Record, Acc).
%% @hidden
sgsn_smt12(#{localSequenceNumber := Sequence} = Record, Acc) ->
	Acc1 = Acc#{<<"localSequenceNumber">> => Sequence},
	sgsn_smt13(Record, Acc1);
sgsn_smt12(Record, Acc) ->
	sgsn_smt13(Record, Acc).
%% @hidden
sgsn_smt13(#{chargingCharacteristics := CC} = Record, Acc) ->
	Acc1 = Acc#{<<"chargingCharacteristics">> => cgf_lib:octet_string(CC)},
	sgsn_smt14(Record, Acc1);
sgsn_smt13(Record, Acc) ->
	sgsn_smt14(Record, Acc).
%% @hidden
sgsn_smt14(#{rATType := RATType} = Record, Acc) ->
	Acc1 = Acc#{<<"rATType">> => RATType},
	sgsn_smt15(Record, Acc1);
sgsn_smt14(Record, Acc) ->
	sgsn_smt15(Record, Acc).
%% @hidden
sgsn_smt15(#{chChSelectionMode := Mode} = Record, Acc) ->
	Acc1 = Acc#{<<"chChSelectionMode">> => Mode},
	sgsn_smt16(Record, Acc1);
sgsn_smt15(Record, Acc) ->
	sgsn_smt16(Record, Acc).
%% @hidden
sgsn_smt16(#{cAMELInformationSMS := CAMELInfo} = Record, Acc) ->
	% Acc1 = Acc#{<<"cAMELInformationSMS">> => camel_info_sms(CAMELInfo)},
	sgsn_smt17(Record, Acc);
sgsn_smt16(Record, Acc) ->
	sgsn_smt17(Record, Acc).
%% @hidden
sgsn_smt17(#{originatingAddress := DN} = Record, Acc) ->
	Acc1 = Acc#{<<"originatingAddress">> => cgf_lib:bcd_dn(DN)},
	sgsn_smt18(Record, Acc1);
sgsn_smt17(Record, Acc) ->
	sgsn_smt18(Record, Acc).
%% @hidden
sgsn_smt18(#{servingNodeType := Type} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodeType">> => Type},
	sgsn_smt19(Record, Acc1);
sgsn_smt18(Record, Acc) ->
	sgsn_smt19(Record, Acc).
%% @hidden
sgsn_smt19(#{servingNodeAddress := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodeAddress">> => cgf_lib:ip_address(Address)},
	sgsn_smt20(Record, Acc1);
sgsn_smt19(Record, Acc) ->
	sgsn_smt20(Record, Acc).
%% @hidden
sgsn_smt20(#{servingNodeiPv6Address := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodeiPv6Address">> => cgf_lib:ip_address(Address)},
	sgsn_smt21(Record, Acc1);
sgsn_smt20(Record, Acc) ->
	sgsn_smt21(Record, Acc).
%% @hidden
sgsn_smt21(#{mMEName := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"mMEName">> => Address},
	sgsn_smt22(Record, Acc1);
sgsn_smt21(Record, Acc) ->
	sgsn_smt22(Record, Acc).
%% @hidden
sgsn_smt22(#{mMERealm := Realm} = Record, Acc) ->
	Acc1 = Acc#{<<"mMERealm">> => Realm},
	sgsn_smt23(Record, Acc1);
sgsn_smt22(Record, Acc) ->
	sgsn_smt23(Record, Acc).
%% @hidden
sgsn_smt23(#{userLocationInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"userLocationInformation">> => cgf_lib:octet_string(Info)},
	sgsn_smt24(Record, Acc1);
sgsn_smt23(Record, Acc) ->
	sgsn_smt24(Record, Acc).
%% @hidden
sgsn_smt24(#{retransmission := _} = Record, Acc) ->
	Acc1 = Acc#{<<"retransmission">> => undefined},
	sgsn_smt25(Record, Acc1);
sgsn_smt24(Record, Acc) ->
	sgsn_smt25(Record, Acc).
%% @hidden
sgsn_smt25(#{servingNodePLMNIdentifier := PLMN} = Record, Acc) ->
	Acc1 = Acc#{<<"servingNodePLMNIdentifier">> => cgf_lib:octet_string(PLMN)},
	sgsn_smt26(Record, Acc1);
sgsn_smt25(Record, Acc) ->
	sgsn_smt26(Record, Acc).
%% @hidden
sgsn_smt26(#{userLocationInfoTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"userLocationInfoTime">> => cgf_lib:bcd_date_time(Time)},
	sgsn_smt27(Record, Acc1);
sgsn_smt26(Record, Acc) ->
	sgsn_smt27(Record, Acc).
%% @hidden
sgsn_smt27(#{cNOperatorSelectionEnt := Entity}, Acc) ->
	Acc#{<<"cNOperatorSelectionEnt">> => Entity};
sgsn_smt27(_Record, Acc) ->
	Acc.

