%%% cgf_cs_huawei.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024-2025 SigScale Global Inc.
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
%%% @doc This library module implements Huawei CS CDR file handling in
%%% 	the {@link //cgf. cgf} application.
%%%
%%% @reference MSOFTX3000 Mobile SoftSwitch Center ASN.1 CDR Description
%%%
-module(cgf_cs_huawei).
-copyright('Copyright (c) 2024-2025 SigScale Global Inc.').

%% export the public API
-export([import/2, import/3]).

%% export the private API
%-export([parse/3]).

-include_lib("kernel/include/logger.hrl").

%%----------------------------------------------------------------------
%%  The cgf_cs_huawei public API
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
			'Huawei-CS':decode('CallEventDataFile', Bin)).
%% @hidden
import2(Log, Metadata, {ok, CallEventDataFile, <<>>}) ->
	import3(Log, Metadata, CallEventDataFile);
import2(Log, Metadata, {ok, CallEventDataFile, Rest}) ->
	?LOG_WARNING([{?MODULE, import},
			{reason, ignored},
			{size, byte_size(Rest)}]),
	import3(Log, Metadata, CallEventDataFile);
import2(_Log, _Metadata, {error, Reason}) ->
	{error, Reason}.
%% @hidden
import3(Log, Metadata, #{callEventRecords := CallEventRecords}) ->
	import4(Log, Metadata, CallEventRecords).
%% @hidden
import4(Log, Metadata, [{moCallRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{forwardCallRecord, FCR} | T]) ->
	case cgf_cs:parse(Log, Metadata, {moCallRecord, FCR}) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{mtCallRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{mtSMSRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{moSMSRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{ssActionRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{outGatewayRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{incGatewayRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{transitRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(Log, Metadata, [{roamingRecord, _} = H | T]) ->
	case cgf_cs:parse(Log, Metadata, H) of
		ok ->
			import4(Log, Metadata, T);
		{error, Reason} ->
			{error, Reason}
	end;
import4(_Log, _Metadata, []) ->
	ok.

%%----------------------------------------------------------------------
%%  The cgf_cs_huawei public API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

