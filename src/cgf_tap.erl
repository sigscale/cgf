%%% cgf_tap.erl
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
%%% @doc This library module implements GSMA TAP file handling in
%%% 	the {@link //cgf. cgf} application.
%%%
-module(cgf_tap).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-export([import/2, import/3]).

-include_lib("kernel/include/logger.hrl").
-include("TAP-0312.hrl").

%%----------------------------------------------------------------------
%%  The cgf_tap public API
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
%% @doc Import CSV file and write to Bx interface log.
import(Filename, Log, Metadata)
		when is_list(Filename), is_list(Metadata) ->
	case file:read_file(Filename) of
		{ok, Bin} ->
			import1(Filename, Log, Metadata,
					'TAP-0312':decode('DataInterChange', Bin));
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
import1(Filename, Log, Metadata, {ok, {transferBatch, TransferBatch}}) ->
	#'TransferBatch'{callEventDetails = CDRs} =  TransferBatch,
	parse(Filename, Log, Metadata, CDRs);
import1(_Filename, _Log, _Metadata, {ok, {notification, _Notification}}) ->
	{error, not_implemented};
import1(Filename, _Log, _Metadata, {error, Reason}) ->
	?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
			{filename, Filename},
			{error, Reason}]).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse(Filename, Log, Metadata, CDRs) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDRs :: [tuple()],
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse CDRs from the import file.
%% @private
parse(Filename, Log, Metadata,
		[{mobileOriginatedCall, MobileOriginatedCall} | T]) ->
	case parse_mo_call(Log, Metadata, MobileOriginatedCall) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{mobileTerminatedCall, MobileTerminatedCall} | T]) ->
	case parse_mt_call(Log, Metadata, MobileTerminatedCall) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{supplServiceEvent, SupplServiceEvent} | T]) ->
	case parse_mmtel(Log, Metadata, SupplServiceEvent) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{serviceCentreUsage, ServiceCentreUsage} | T]) ->
	case parse_sc_sm(Log, Metadata, ServiceCentreUsage) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{gprsCall, GprsCall} | T]) ->
	case parse_gprs(Log, Metadata, GprsCall) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{contentTransaction, ContentTransaction} | T]) ->
	case parse_content(Log, Metadata, ContentTransaction) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{locationService, LocationService} | T]) ->
	case parse_location(Log, Metadata, LocationService) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{messagingEvent, MessagingEvent} | T]) ->
	case parse_message(Log, Metadata, MessagingEvent) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{mobileSession, MobileSession} | T]) ->
	case parse_session(Log, Metadata, MobileSession) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(_Filename, _Log, _Metadata, []) ->
	ok.

-spec parse_mo_call(Log, Metadata, MobileOriginatedCall) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MobileOriginatedCall :: #'MobileOriginatedCall'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile originated call.
parse_mo_call(_Log, _Metadata, _MobileOriginatedCall) ->
	{error, not_implemented}.

-spec parse_mt_call(Log, Metadata, MobileTerminatedCall) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MobileTerminatedCall :: #'MobileTerminatedCall'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile terminated call.
parse_mt_call(_Log, _Metadata, _MobileTerminatedCall) ->
	{error, not_implemented}.

-spec parse_mmtel(Log, Metadata, SupplServiceEvent) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SupplServiceEvent :: #'SupplServiceEvent'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a supplementary service.
parse_mmtel(_Log, _Metadata, _SupplServiceEvent) ->
	{error, not_implemented}.

-spec parse_sc_sm(Log, Metadata, ServiceCentreUsage) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ServiceCentreUsage :: #'ServiceCentreUsage'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a service center usage.
parse_sc_sm(_Log, _Metadata, _ServiceCentreUsage) ->
	{error, not_implemented}.

-spec parse_gprs(Log, Metadata, GprsCall) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		GprsCall :: #'GprsCall'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a GPRS session.
parse_gprs(_Log, _Metadata, _GprsCall) ->
	{error, not_implemented}.

-spec parse_content(Log, Metadata, ContentTransaction) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ContentTransaction :: #'ContentTransaction'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a content transaction.
parse_content(_Log, _Metadata, _ContentTransaction) ->
	{error, not_implemented}.

-spec parse_location(Log, Metadata, LocationService) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		LocationService :: #'LocationService'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a location service.
parse_location(_Log, _Metadata, _LocationService) ->
	{error, not_implemented}.

-spec parse_message(Log, Metadata, MessagingEvent) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MessagingEvent :: #'MessagingEvent'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a messaging event.
parse_message(_Log, _Metadata, _MessagingEvent) ->
	{error, not_implemented}.

-spec parse_session(Log, Metadata, MobileSession) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MobileSession :: #'MobileSession'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile session.
parse_session(_Log, _Metadata, _MobileSession) ->
	{error, not_implemented}.

