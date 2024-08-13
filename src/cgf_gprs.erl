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
parse_sgsn_pdp(_Log, _Metadata, _SGSNPDPRecord) ->
	{error, not_implemented}.

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

