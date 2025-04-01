%%% cgf_cs_huawei_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2025 SigScale Global Inc.
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
%%% @doc This {@link //cgf/cgf_import_fsm. cgf_import_fsm} callback module
%%% 	implements a Huawei circuit switched (CS) CDR import FSM within
%%% 	the {@link //cgf. cgf} application.
%%%
%%% 	This call back module handles reading a file of concatenated
%%% 	ASN.1 BER encoded Huawei CS CDR, parsing the charging data records
%%% 	and logging to a JSON Bx interface log.
%%%
%%% 	== State Transitions ==
%%% 	The following diagram depicts the states, and events which drive
%%% 	state transitions, in the finite state machine (FSM):
%%%
%%% 	<img alt="state machine" src="import-fsm.svg" />
%%%
-module(cgf_cs_huawei_fsm).
-copyright('Copyright (c) 2025 SigScale Global Inc.').

%% export the cgf_import_fsm callback API
-export([init/1, open/2, read/2, parse/3, close/2]).

-behaviour(cgf_import_fsm).

-export_type([cont/0]).

-opaque cont() :: binary().

-type record_type() :: moCallRecord | mtCallRecord
				| moSMSRecord | mtSMSRecord
				| ssActionRecord | incGatewayRecord
				| outGatewayRecord | transitRecord
				| roamingRecord | forwardCallRecord.

-type statedata() ::
		#{filename := file:filename() | binary(),
		log := disk_log:log(),
		metadata => [{AttributeName :: string(),
				AttributeValue :: term()}],
		extra_args => [term()],
		read := non_neg_integer(),
		parsed := #{RecordType :: record_type() => non_neg_integer()}}.

%%----------------------------------------------------------------------
%%  The cgf_import_fsm callback API
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateData} | {error, Reason},
		StateData :: statedata(),
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
init([Filename, Log] = _Args) ->
	init([Filename, Log, #{}]);
init([Filename, Log, Metadata | ExtraArgs] = _Args) ->
	NewMetadata = metadata(Filename, Metadata),
	StateData = #{filename => Filename, log => Log,
			metadata => maps:to_list(NewMetadata),
			extra_args => ExtraArgs,
			read => 0, parsed => #{}},
	{ok, StateData}.

-spec open(Filename, StateData) -> Result
	when
		Filename :: file:filename() | binary(),
		StateData :: statedata(),
		Result :: {continue, Cont, StateData}
				| {error, Reason},
		Cont :: cont(),
		Reason :: term().
%% @doc Handles events received in the <em>open</em> state.
%%
%% 	Open the `Filename' for subsequent reading.
%% @todo Read file in chunks.
open(Filename, StateData) ->
	case file:read_file(Filename) of
		{ok, Cont} ->
			{continue, Cont, StateData};
		{error, Reason} ->
			{error, Reason}
	end.

-spec read(Cont, StateData) -> Result
	when
		Cont :: cont(),
		StateData :: statedata(),
		Result :: {continue, CDR, Cont, StateData}
				| {error, Reason, Cont, StateData}
				| {close, Reason, Cont, StateData},
		CDR :: term(),
		Reason :: asn1_decode | normal | shutdown | term().
%% @doc Handles events received in the <em>read</em> state.
%%
%% 	Read a charging data record from the CDR file.
%%
read(<<>> = Cont, StateData) ->
	{close, normal, Cont, StateData};
read(Cont, StateData) ->
	case 'Huawei-CS':decode('CallEventDataFile', Cont) of
		{ok, CDR, Cont1} ->
			F = fun(Count) -> Count + 1 end,
			NewStateData = maps:update_with(read, F, StateData),
			{continue, CDR, Cont1, NewStateData};
		{error, {asn1, _Description}} ->
			{close, asn1_decode, <<>>, StateData}
	end.

-spec parse(CDR, Log, StateData) -> Result
	when
		CDR :: {RecordType, Record},
		RecordType :: record_type(),
		Record :: map(),
		Log :: disk_log:log(),
		StateData :: statedata(),
		Result :: {continue, StateData}
				| {error, Reason, StateData}
				| {close, Reason, StateData}
				| {stop, Reason, StateData},
		Reason :: normal | shutdown | term().
%% @doc Handles events received in the <em>parse</em> state.
%%
%% 	Parse and log a charging data record (CDR).
%%
parse({forwardCallRecord, FCR} = _CDR, Log,
		#{metadata := Metadata, parsed := Parsed} = StateData) ->
	case cgf_cs:parse(Log, Metadata, {moCallRecord, FCR}) of
		ok ->
			F = fun(Count) -> Count + 1 end,
			Parsed1 = maps:update_with(forwardCallRecord, F, 1, Parsed),
			NewStateData = StateData#{parsed => Parsed1},
			{continue, NewStateData};
		{error, Reason} ->
			{error, Reason, StateData}
	end;
parse({RecordType, _Record} = CDR, Log,
		#{metadata := Metadata, parsed := Parsed} = StateData) ->
	case cgf_cs:parse(Log, Metadata, CDR) of
		ok ->
			F = fun(Count) -> Count + 1 end,
			Parsed1 = maps:update_with(RecordType, F, 1, Parsed),
			NewStateData = StateData#{parsed => Parsed1},
			{continue, NewStateData};
		{error, Reason} ->
			{error, Reason, StateData}
	end.

-spec close(Cont, StateData) -> Result
	when
		Cont :: cont(),
		StateData :: statedata(),
		Result :: {stop, Report, StateData}
				| {error, Reason, StateData},
		Report :: map(),
		Reason :: normal | shutdown | term().
%% @doc Handles events received in the <em>close</em> state.
close(_Cont, StateData) ->
	{stop, report(StateData), StateData}.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
metadata(Filename, Metadata) ->
	MimeType = "application/octet-stream",
	FileMap = #{"name" => filename:basename(Filename),
			"mime_type" => MimeType},
	metadata1(FileMap, Metadata).
%% @hidden
metadata1(FileMap, #{"log" := MetaLog} = Metadata) ->
	F = fun("file", MetaValue, FileValue) ->
				maps:merge(MetaValue, FileValue);
			(_, MetaValue, _) ->
				MetaValue
	end,
	MetaLog1 = maps:merge_with(F, MetaLog, #{"file" => FileMap}),
	Metadata#{"log" => MetaLog1};
metadata1(FileMap, Metadata) ->
	Metadata#{"log" => #{"file" => FileMap}}.

%% @hidden
report(#{read := Read, parsed := Parsed} = _Statedata) ->
	F = fun(moCallRecord, Count, Acc) ->
				Acc#{<<"moCall">> => Count};
			(mtCallRecord, Count, Acc) ->
				Acc#{<<"mtCall">> => Count};
			(moSMSRecord, Count, Acc) ->
				Acc#{<<"moSMS">> => Count};
			(mtSMSRecord, Count, Acc) ->
				Acc#{<<"mtSMS">> => Count};
			(ssActionRecord, Count, Acc) ->
				Acc#{<<"ssAction">> => Count};
			(incGatewayRecord, Count, Acc) ->
				Acc#{<<"incGateway">> => Count};
			(outGatewayRecord, Count, Acc) ->
				Acc#{<<"outGateway">> => Count};
			(transitRecord, Count, Acc) ->
				Acc#{<<"transit">> => Count};
			(roamingRecord, Count, Acc) ->
				Acc#{<<"roaming">> => Count};
			(forwardCallRecord, Count, Acc) ->
				Acc#{<<"forwardCall">> => Count}
	end,
	Counts = maps:fold(F, #{}, Parsed),
	#{<<"totalRecords">> => Read, <<"loggedCount">> => Counts}.

