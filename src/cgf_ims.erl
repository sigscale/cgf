%%% cgf_ims.erl
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
%%% @doc This library module implements 3GPP CS CDR file handling in
%%% 	the {@link //cgf. cgf} application.
%%%
%%% @reference 3GPP TS <a
%%% 	href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=32&amp;GSMSpecPart2=298"
%%% 	>32.298</a> Charging Data Record (CDR) Parameter Description.
%%%
-module(cgf_ims).
-copyright('Copyright (c) 2024 SigScale Global Inc.').

%% export the public API
-export([import/2, import/3]).

%% export the private API
-export([parse/3]).

-include_lib("kernel/include/logger.hrl").

%%----------------------------------------------------------------------
%%  The cgf_ims public API
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
			'IMSChargingDataTypes':decode('IMSRecord', Bin)).

%% @hidden
import2(Log, Metadata, {ok, CDR, Rest}) ->
	case parse(Log, Metadata, CDR) of
		ok when byte_size(Rest) == 0 ->
			ok;
		_ ->
			import1(Log, Metadata, Rest)
	end;
import2(_Log, _Metadata, {error, Reason}) ->
	{error, Reason}.

%%----------------------------------------------------------------------
%%  The cgf_ims public API
%%----------------------------------------------------------------------

-spec parse(Log, Metadata, CDR) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDR :: {RecordType, Record},
		RecordType :: sCSCFRecord | pCSCFRecord | iCSCFRecord |
				mRFCRecord | mGCFRecord | bGCFRecord | aSRecord |
				eCSCFRecord | iBCFRecord | tRFRecord | tFRecord |
				aTCFRecord,
		Record :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR.
%% @private
parse(Log, Metadata, {sCSCFRecord, SCSCFRecord} = _CDR) ->
	case parse_scscf(Log, Metadata, SCSCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_scscf},
					{error, Reason}])
	end;
parse(Log, Metadata, {pCSCFRecord, PCSCFRecord}) ->
	case parse_pcscf(Log, Metadata, PCSCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_pcscf},
					{error, Reason}])
	end;
parse(Log, Metadata, {iCSCFRecord, ICSCFRecord}) ->
	case parse_icscf(Log, Metadata, ICSCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_icscf},
					{error, Reason}])
	end;
parse(Log, Metadata, {mRFCRecord, MRFCRecord}) ->
	case parse_mrfc(Log, Metadata, MRFCRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mrfc},
					{error, Reason}])
	end;
parse(Log, Metadata, {mGCFRecord, MGCFRecord}) ->
	case parse_mgcf(Log, Metadata, MGCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mgcf},
					{error, Reason}])
	end;
parse(Log, Metadata, {bGCFRecord, BGCFRecord}) ->
	case parse_bgcf(Log, Metadata, BGCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_bgcf},
					{error, Reason}])
	end;
parse(Log, Metadata, {aSRecord, ASRecord}) ->
	case parse_as(Log, Metadata, ASRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_as},
					{error, Reason}])
	end;
parse(Log, Metadata, {eCSCFRecord, ECSCFRecord}) ->
	case parse_ecscf(Log, Metadata, ECSCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_ecscf},
					{error, Reason}])
	end;
parse(Log, Metadata, {iBCFRecord, IBCFRecord}) ->
	case parse_ibcf(Log, Metadata, IBCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_ibcf},
					{error, Reason}])
	end;
parse(Log, Metadata, {tRFRecord, TRFRecord}) ->
	case parse_trf(Log, Metadata, TRFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_trf},
					{error, Reason}])
	end;
parse(Log, Metadata, {tFRecord, TFRecord}) ->
	case parse_tf(Log, Metadata, TFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_trf},
					{error, Reason}])
	end;
parse(Log, Metadata, {aTCFRecord, ATCFRecord}) ->
	case parse_atcf(Log, Metadata, ATCFRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_atcf},
					{error, Reason}])
	end.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse_scscf(Log, Metadata, SCSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SCSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SCSCFRecord.
parse_scscf(_Log, _Metadata, _SCSCFRecord) ->
	{error, not_implemented}.

-spec parse_pcscf(Log, Metadata, PCSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		PCSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an PCSCFRecord.
parse_pcscf(_Log, _Metadata, _PCSCFRecord) ->
	{error, not_implemented}.

-spec parse_icscf(Log, Metadata, ICSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ICSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ICSCFRecord.
parse_icscf(_Log, _Metadata, _ICSCFRecord) ->
	{error, not_implemented}.

-spec parse_mrfc(Log, Metadata, MRFCRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MRFCRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MRFCRecord.
parse_mrfc(_Log, _Metadata, _MRFCRecord) ->
	{error, not_implemented}.

-spec parse_mgcf(Log, Metadata, MGCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MGCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MGCFRecord.
parse_mgcf(_Log, _Metadata, _MGCFRecord) ->
	{error, not_implemented}.

-spec parse_bgcf(Log, Metadata, BGCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		BGCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an BGCFRecord.
parse_bgcf(_Log, _Metadata, _BGCFRecord) ->
	{error, not_implemented}.

-spec parse_as(Log, Metadata, ASRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ASRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ASRecord.
parse_as(_Log, _Metadata, _ASRecord) ->
	{error, not_implemented}.

-spec parse_ecscf(Log, Metadata, ECSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ECSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ECSCFRecord.
parse_ecscf(_Log, _Metadata, _ECSCFRecord) ->
	{error, not_implemented}.

-spec parse_ibcf(Log, Metadata, IBCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		IBCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an IBCFRecord.
parse_ibcf(_Log, _Metadata, _IBCFRecord) ->
	{error, not_implemented}.

-spec parse_trf(Log, Metadata, TRFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		TRFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an TRFRecord.
parse_trf(_Log, _Metadata, _TRFRecord) ->
	{error, not_implemented}.

-spec parse_tf(Log, Metadata, TFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		TFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an TFRecord.
parse_tf(_Log, _Metadata, _TFRecord) ->
	{error, not_implemented}.

-spec parse_atcf(Log, Metadata, ATCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ATCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ATCFRecord.
parse_atcf(_Log, _Metadata, _ATCFRecord) ->
	{error, not_implemented}.

