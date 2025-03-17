%%% cgf_cs_fsm.erl
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
%%% 	implements a circuit switched (CS) CDR import FSM within
%%% 	the {@link //cgf. cgf} application.
%%%
%%% 	This call back module handles reading a file of concatenated
%%% 	ASN.1 BER encoded CS CDR, parsing the charging data records
%%% 	and logging to a JSON Bx interface log.
%%%
%%% 	== State Transitions ==
%%% 	The following diagram depicts the states, and events which drive
%%% 	state transitions, in the finite state machine (FSM):
%%%
%%% 	<img alt="state machine" src="import-fsm.svg" />
%%%
-module(cgf_cs_fsm).
-copyright('Copyright (c) 2025 SigScale Global Inc.').

%% export the cgf_import_fsm callback API
-export([init/1, open/2, read/2, parse/3, close/2]).

-behaviour(cgf_import_fsm).

-export_type([cont/0]).

-opaque cont() :: binary().

-type statedata() ::
		#{filename := file:filename() | binary(),
		log := disk_log:log(),
		metadata => [{AttributeName :: string(),
				AttributeValue :: term()}],
		extra_args => [term()]}.

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
			extra_args => ExtraArgs},
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
	case 'CSChargingDataTypes':decode('CSRecord', Cont) of
		{ok, CDR, Cont1} ->
			{continue, CDR, Cont1, StateData};
		{error, {asn1, _Description}} ->
			{close, asn1_decode, <<>>, StateData}
	end.

-spec parse(CDR, Log, StateData) -> Result
	when
		CDR :: term(),
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
parse(CDR, Log, #{metadata := Metadata} = StateData) ->
	case cgf_cs:parse(Log, Metadata, CDR) of
		ok ->
			{continue, StateData};
		{error, Reason} ->
			{error, Reason, StateData}
	end.

-spec close(Cont, StateData) -> Result
	when
		Cont :: cont(),
		StateData :: statedata(),
		Result :: {stop, StateData}
				| {error, Reason, StateData},
		Reason :: normal | shutdown | term().
%% @doc Handles events received in the <em>close</em> state.
close(_Cont, StateData) ->
	{stop, StateData}.

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

