%%% cgf_cs.erl
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
-module(cgf_cs).
-copyright('Copyright (c) 2024 SigScale Global Inc.').

-export([import/2, import/3, parse/3]).

-include_lib("kernel/include/logger.hrl").

%%----------------------------------------------------------------------
%%  The cgf_cs public API
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
			'CSChargingDataTypes':decode('CSRecord', Bin)).

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
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse(Log, Metadata, CDR) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDR :: {RecordType, Record},
		RecordType :: moCallRecord | mtCallRecord | moSMSRecord
				| mtSMSGWRecord,
		Record :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR.
%% @private
parse(Log, Metadata, {moCallRecord, MOCallRecord} = _CDR) ->
	case parse_mo_call(Log, Metadata, MOCallRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mo_call},
					{error, Reason}])
	end;
parse(Log, Metadata, {mtCallRecord, MTCallRecord}) ->
	case parse_mt_call(Log, Metadata, MTCallRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mt_call},
					{error, Reason}])
	end;
parse(Log, Metadata, {moSMSRecord, MOSMSRecord}) ->
	case parse_mo_sms(Log, Metadata, MOSMSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mo_sms},
					{error, Reason}])
	end;
parse(Log, Metadata, {mtSMSGWRecord, MTSMSGWRecord}) ->
	case parse_mt_smsgw(Log, Metadata, MTSMSGWRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mt_smsgw},
					{error, Reason}])
	end.

-spec parse_mo_call(Log, Metadata, MOCallRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MOCallRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MO Call Record.
parse_mo_call(_Log, _Metadata, _MOCallRecord) ->
	{error, not_implemented}.

-spec parse_mt_call(Log, Metadata, MTCallRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MTCallRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MT Call Record.
parse_mt_call(_Log, _Metadata, _MTCallRecord) ->
	{error, not_implemented}.

-spec parse_mo_sms(Log, Metadata, MOSMSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MOSMSRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MO SMS Record.
parse_mo_sms(_Log, _Metadata, _MOSMSRecord) ->
	{error, not_implemented}.

-spec parse_mt_smsgw(Log, Metadata, MTSMSGWRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MTSMSGWRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MT SMSGW Record.
parse_mt_smsgw(_Log, _Metadata, _MTSMSGWRecord) ->
	{error, not_implemented}.

