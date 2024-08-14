%%% cgf_gprs.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 SigScale Global Inc.
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
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-export([import/2, import/3]).

-include_lib("kernel/include/logger.hrl").

%%----------------------------------------------------------------------
%%  The cgf_gprs public API
%%----------------------------------------------------------------------

-spec import(Filename, Log) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv import(Filename, Log, [])
import(Filename, Log) ->
	import(Filename, Log, []).

-spec import(Filename, Log, Metadata) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Import CDR file and write to Bx interface log.
import(Filename, Log, Metadata)
		when is_list(Filename), is_list(Metadata) ->
	case file:read_file(Filename) of
		{ok, Bin} ->
			import1(Filename, Log, Metadata,
					'GPRSChargingDataTypes':decode('GPRSRecord', Bin));
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, import},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
import1(Filename, Log, Metadata, {ok, CDR, Rest}) ->
	case Rest of
		<<>> ->
			ok;
		_ ->
			?LOG_WARNING([{?MODULE, import},
					{reason, ignored},
					{size, byte_size(Rest)}])
	end,
	case parse(Filename, Log, Metadata, CDR) of
		ok ->
			import1(Filename, Log, Metadata,
					'GPRSChargingDataTypes':decode('GPRSRecord', Rest));
		{error, Reason} ->
			{error, Reason}
	end;
import1(Filename, _Log, _Metadata, {error, Reason}) ->
	?LOG_ERROR([{?MODULE, import},
			{filename, Filename},
			{error, Reason}]).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse(Filename, Log, Metadata, CDR) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDR :: {RecordType, Record},
		RecordType :: sgsnPDPRecord | sgsnMMRecord | sgsnSMORecord
				| sgsnSMTRecord | sgsnMTLCSRecord | sgsnMOLCSRecord
				| sgsnNILCSRecord | sgsnMBMSRecord | ggsnMBMSRecord
				| sGWRecord | pGWRecord | gwMBMSRecord | tDFRecord
				| iPERecord | ePDGRecord | tWAGRecord,
		Record :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse CDRs from the import file.
%% @private
parse(Filename, Log, Metadata,
		{sgsnPDPRecord, SGSNPDPRecord} = _CDR) ->
	case parse_sgsn_pdp(Log, Metadata, SGSNPDPRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_pdp},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sgsnMMRecord, SGSNMMRecord}) ->
	case parse_sgsn_mmr(Log, Metadata, SGSNMMRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_mmr},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sgsnSMORecord, SGSNSMORecord}) ->
	case parse_sgsn_smo(Log, Metadata, SGSNSMORecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_smo},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sgsnSMTRecord, SGSNSMTRecord}) ->
	case parse_sgsn_smt(Log, Metadata, SGSNSMTRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_smt},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sgsnMTLCSRecord, SGSNMTLCSRecord}) ->
	case parse_sgsn_mt_lcs(Log, Metadata, SGSNMTLCSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_mt_lcs},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sgsnMOLCSRecord, SGSNMOLCSRecord}) ->
	case parse_sgsn_mo_lcs(Log, Metadata, SGSNMOLCSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_mo_lcs},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sgsnNILCSRecord, SGSNNILCSRecord}) ->
	case parse_sgsn_ni_lcs(Log, Metadata, SGSNNILCSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_ni_lcs},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sgsnMBMSRecord, SGSNMBMSRecord}) ->
	case parse_sgsn_mbms(Log, Metadata, SGSNMBMSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgsn_mbms},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{ggsnMBMSRecord, GGSNMBMSRecord}) ->
	case parse_ggsn_mbms(Log, Metadata, GGSNMBMSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_ggsn_mbms},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{sGWRecord, SGWRecord}) ->
	case parse_sgw(Log, Metadata, SGWRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sgw},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{pGWRecord, PGWRecord}) ->
	case parse_pgw(Log, Metadata, PGWRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_pgw},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{gwMBMSRecord, GWMBMSRecord}) ->
	case parse_gw_mbms(Log, Metadata, GWMBMSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_gw_mbms},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{tDFRecord, TDFRecord}) ->
	case parse_tdf(Log, Metadata, TDFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_tdf},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{iPERecord, IPERecord}) ->
	case parse_ipe(Log, Metadata, IPERecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_ipe},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{ePDGRecord, EPDGRecord}) ->
	case parse_epdg(Log, Metadata, EPDGRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_epdg},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		{tWAGRecord, TWAGRecord}) ->
	case parse_twag(Log, Metadata, TWAGRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_twag},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end.

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
	CDR = [{sgsn_pdp, Call} | Metadata],
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
parse_sgsn_mmr(_Log, _Metadata, _SGSNMMRecord) ->
	{error, not_implemented}.

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
parse_sgsn_smo(_Log, _Metadata, _SGSNSMORecord) ->
	{error, not_implemented}.

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
parse_sgsn_smt(_Log, _Metadata, _SGSNSMTRecord) ->
	{error, not_implemented}.

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
parse_pgw(_Log, _Metadata, _PGWRecord) ->
	{error, not_implemented}.

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
	Acc1 = Acc#{<<"cellIdentifier">> => con_string(CI)},
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
	Acc1 = Acc#{<<"chargingCharacteristics">> => con_string(CC)},
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
sgsn_pdp_record8(#{diagnostics
		:= Dia} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => Dia},
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
sgsn_pdp_record11(#{ggsnAddressUsed
		:= {_, {_, Address}}} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"ggsnAddressUsed">> => Address},
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
	Acc1 = Acc#{<<"locationAreaCode">> => con_string(LocationAreaCode)},
	sgsn_pdp_record15(SGSNPDPRecord, Acc1);
sgsn_pdp_record14(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record15(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record15(#{mSNetworkCapability
		:= MSNetworkCapability} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"mSNetworkCapability">> =>
			cgf_lib:octet_string(MSNetworkCapability)},
	sgsn_pdp_record16(SGSNPDPRecord, Acc1);
sgsn_pdp_record15(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record16(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record16(#{lowPriorityIndicator
		:= LowPriorityIndicator} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"lowPriorityIndicator">> => LowPriorityIndicator},
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
	Acc1 = Acc#{<<"recordOpeningTime">> => bcd(RecordOpeningTime)},
	sgsn_pdp_record23(SGSNPDPRecord, Acc1);
sgsn_pdp_record22(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record23(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record23(#{recordType
		:= RecordType} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"recordType">> => RecordType},
	sgsn_pdp_record24(SGSNPDPRecord, Acc1);
sgsn_pdp_record23(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record24(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record24(#{routingArea
		:= RoutingArea} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"routingArea">> => con_string(RoutingArea)},
	sgsn_pdp_record25(SGSNPDPRecord, Acc1);
sgsn_pdp_record24(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record25(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record25(#{servedIMEI
		:= IMEI} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => bcd(IMEI)},
	sgsn_pdp_record26(SGSNPDPRecord, Acc1);
sgsn_pdp_record25(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record26(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record26(#{servedIMSI
		:= IMSI} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMSI">> => bcd(IMSI)},
	sgsn_pdp_record27(SGSNPDPRecord, Acc1);
sgsn_pdp_record26(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record27(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record27(#{servedMSISDN
		:= MSISDN} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedMSISDN">> => bcd(MSISDN)},
	sgsn_pdp_record28(SGSNPDPRecord, Acc1);
sgsn_pdp_record27(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record28(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record28(#{servedPDPAddress := {iPAddress,
		{_, {_, IPAddress}}}} = SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servedPDPAddress">> => IPAddress},
	sgsn_pdp_record29(SGSNPDPRecord, Acc1);
sgsn_pdp_record28(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record29(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record29(#{servedPDPPDNAddressExt := GSNAddressList}
		= SGSNPDPRecord, Acc) ->
	ParsedAddressList = [to_ipv4(GSNAddress) || {_,{iPBinV4Address ,GSNAddress}} <- GSNAddressList],
	Acc1 = Acc#{<<"servedPDPPDNAddressExt">> => ParsedAddressList},
	sgsn_pdp_record30(SGSNPDPRecord, Acc1);
sgsn_pdp_record29(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record30(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record30(#{servingNodePLMNIdentifier := Identifer}
		= SGSNPDPRecord, Acc) ->
	Acc1 = Acc#{<<"servingNodePLMNIdentifier">> => con_string(Identifer)},
	sgsn_pdp_record31(SGSNPDPRecord, Acc1);
sgsn_pdp_record30(SGSNPDPRecord, Acc) ->
	sgsn_pdp_record31(SGSNPDPRecord, Acc).
%% @hidden
sgsn_pdp_record31(#{sgsnAddress := AddressList}
		= SGSNPDPRecord, Acc) ->
	ParsedAddressList = [to_ipv4(Address) || {_,{iPBinV4Address ,Address}} <- AddressList],
	Acc1 = Acc#{<<"sgsnAddress">> => ParsedAddressList},
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
traffic_volumes2(#{changeCondition
		:= CC} = TV, Acc) ->
	Acc1 = Acc#{<<"changeCondition">> => CC},
	traffic_volumes3(TV, Acc1);
traffic_volumes2(TV, Acc) ->
	traffic_volumes3(TV, Acc).
%% @hidden
traffic_volumes3(#{changeTime
		:= CT} = TV, Acc) ->
	Acc1 = Acc#{<<"changeTime">> => bcd(CT)},
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
traffic_volumes7(#{diagnostics
		:= DIA} = TV, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => DIA},
	traffic_volumes8(TV, Acc1);
traffic_volumes7(TV, Acc) ->
	traffic_volumes8(TV, Acc).
%% @hidden
traffic_volumes8(#{diagnostics
		:= DIA} = TV, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => DIA},
	traffic_volumes9(TV, Acc1);
traffic_volumes8(TV, Acc) ->
	traffic_volumes9(TV, Acc).
%% @hidden
traffic_volumes9(#{ePCQoSInformation
		:= EPCQoSInformation} = TV, Acc) ->
	Acc1 = Acc#{<<"ePCQoSInformation">> => EPCQoSInformation},
	traffic_volumes10(TV, Acc1);
traffic_volumes9(TV, Acc) ->
	traffic_volumes10(TV, Acc).
%% @hidden
traffic_volumes10(#{enhancedDiagnostics
		:= EnhancedDiagnostics} = TV, Acc) ->
	Acc1 = Acc#{<<"enhancedDiagnostics">> => EnhancedDiagnostics},
	traffic_volumes11(TV, Acc1);
traffic_volumes10(TV, Acc) ->
	traffic_volumes11(TV, Acc).
%% @hidden
traffic_volumes11(#{listOfPresenceReportingAreaInformation
		:= ListOfPRA} = TV, Acc) ->
	Acc1 = Acc#{<<"listOfPresenceReportingAreaInformation">> => ListOfPRA},
	traffic_volumes12(TV, Acc1);
traffic_volumes11(TV, Acc) ->
	traffic_volumes12(TV, Acc).
%% @hidden
traffic_volumes12(#{qosNegotiated
		:= QOSNegotiated} = TV, Acc) ->
	Acc1 = Acc#{<<"qosNegotiated">> => cgf_lib:octet_string(QOSNegotiated)},
	traffic_volumes13(TV, Acc1);
traffic_volumes12(TV, Acc) ->
	traffic_volumes13(TV, Acc).
%% @hidden
traffic_volumes13(#{qosRequested
		:= QOSRequested} = TV, Acc) ->
	Acc1 = Acc#{<<"qosRequested">> => cgf_lib:octet_string(QOSRequested)},
	traffic_volumes14(TV, Acc1);
traffic_volumes13(TV, Acc) ->
	traffic_volumes14(TV, Acc).
%% @hidden
traffic_volumes14(#{relatedChangeOfCharCondition
		:= ChangeOfCharCon} = TV, Acc) ->
	Acc1 = Acc#{<<"relatedChangeOfCharCondition">> => ChangeOfCharCon},
	traffic_volumes15(TV, Acc1);
traffic_volumes14(TV, Acc) ->
	traffic_volumes15(TV, Acc).
%% @hidden
traffic_volumes15(#{servingPLMNRateControl
		:= RateControl} = TV, Acc) ->
	Acc1 = Acc#{<<"servingPLMNRateControl">> => RateControl},
	traffic_volumes16(TV, Acc1);
traffic_volumes15(TV, Acc) ->
	traffic_volumes16(TV, Acc).
%% @hidden
traffic_volumes16(#{threeGPPPSDataOffStatus
		:= DataOffStatus} = TV, Acc) ->
	Acc1 = Acc#{<<"threeGPPPSDataOffStatus">> => DataOffStatus},
	traffic_volumes17(TV, Acc1);
traffic_volumes16(TV, Acc) ->
	traffic_volumes17(TV, Acc).
%% @hidden
traffic_volumes17(#{uWANUserLocationInformation
		:= WanUserLocInfo} = TV, Acc) ->
	Acc1 = Acc#{<<"uWANUserLocationInformation">> => WanUserLocInfo},
	traffic_volumes18(TV, Acc1);
traffic_volumes17(TV, Acc) ->
	traffic_volumes18(TV, Acc).
%% @hidden
traffic_volumes18(#{userCSGInformation
		:= CSGInfo} = TV, Acc) ->
	Acc1 = Acc#{<<"userCSGInformation">> => CSGInfo},
	traffic_volumes19(TV, Acc1);
traffic_volumes18(TV, Acc) ->
	traffic_volumes19(TV, Acc).
%% @hidden
traffic_volumes19(#{userLocationInformation
		:= UserLocInfo} = _TV, Acc) ->
	Acc#{<<"userLocationInformation">> => UserLocInfo};
traffic_volumes19(_TV, Acc) ->
	Acc.
%% To-Do Add RATType into traffic list

%% @hidden
bcd(Binary) ->
	bcd(Binary, []).
%% @hidden
bcd(<<D:4, 15:4>>, Acc) ->
	bcd1([D | Acc], []);
bcd(<<D1:4, D2:4>>, Acc) ->
	bcd1([D2, D1 | Acc], []);
bcd(<<D1:4, D2:4, Rest/binary>>, Acc) ->
	bcd(Rest, [D2, D1 | Acc]).
%% @hidden
bcd1([0 | T], Acc) ->
	bcd1(T, [$0 | Acc]);
bcd1([1 | T], Acc) ->
	bcd1(T, [$1 | Acc]);
bcd1([2 | T], Acc) ->
	bcd1(T, [$2 | Acc]);
bcd1([3 | T], Acc) ->
	bcd1(T, [$3 | Acc]);
bcd1([4 | T], Acc) ->
	bcd1(T, [$4 | Acc]);
bcd1([5 | T], Acc) ->
	bcd1(T, [$5 | Acc]);
bcd1([6 | T], Acc) ->
	bcd1(T, [$6 | Acc]);
bcd1([7 | T], Acc) ->
	bcd1(T, [$7 | Acc]);
bcd1([8 | T], Acc) ->
	bcd1(T, [$8 | Acc]);
bcd1([9 | T], Acc) ->
	bcd1(T, [$9 | Acc]);
bcd1([10 | T], Acc) ->
	bcd1(T, [$a | Acc]);
bcd1([11 | T], Acc) ->
	bcd1(T, [$b | Acc]);
bcd1([12 | T], Acc) ->
	bcd1(T, [$c | Acc]);
bcd1([13 | T], Acc) ->
	bcd1(T, [$d | Acc]);
bcd1([14 | T], Acc) ->
	bcd1(T, [$e | Acc]);
bcd1([15 | T], Acc) ->
	bcd1(T, [$f | Acc]);
bcd1([], Acc) ->
	Acc.

%% @hidden
to_ipv4(<<A:8, B:8, C:8, D:8>>) ->
	integer_to_list(A) ++ [$.] ++
		integer_to_list(B) ++ [$.] ++
		integer_to_list(C) ++ [$.] ++
		integer_to_list(D).

%% @hidden
con_string(Binary) when is_binary(Binary) ->
	con_string(Binary, []).
%% @hidden
con_string(<<Byte:8, Rest/binary>>, Acc) ->
	NewAcc = [integer_to_list(Byte) | Acc],
	con_string(Rest, NewAcc);
con_string(<<>>, Acc) ->
	lists:flatten(lists:reverse(Acc)).

